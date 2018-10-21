(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload :cl-cpp-generator))

;;https://github.com/halide/Halide/blob/5d298f0d1731e5b89ee6ff1ae948a5201e6ea471/test/correctness/fit_function.cpp

(in-package :cl-cpp-generator)

;(push :cxxopts *features*)

(defmacro e (&body body)
  `(statements (<< "std::cout" ,@(loop for e in body collect
                                      (cond ((stringp e) `(string ,e))
                                            (t e))) "std::endl")))

(defmacro er (&body body)
  `(statements (<< "std::cerr" ,@(loop for e in body collect
                                      (cond ((stringp e) `(string ,e))
                                            (t e))) "std::endl")))

(defun replace-all (string part replacement &key (test #'char=))
"Returns a new string in which all the occurences of the part 
is replaced with replacement."
    (with-output-to-string (out)
      (loop with part-length = (length part)
            for old-pos = 0 then (+ pos part-length)
            for pos = (search part string
                              :start2 old-pos
                              :test test)
            do (write-string string out
                             :start old-pos
                             :end (or pos (length string)))
            when pos do (write-string replacement out)
            while pos))) 


(defun dox (&key brief usage params return)
  `(
    (raw " ")
    (raw ,(format nil "//! @brief ~a" brief)) (raw "//! ")
    (raw ,(format nil "//! @usage ~a" usage)) (raw "//! ")
    ,@(loop for (name desc) in params collect
         `(raw ,(format nil "//! @param ~a ~a" name desc)))
    (raw "//! ")
    (raw ,(format nil "//! @return ~a" return))
    (raw " ")))


(defparameter *halide-facts*
  `((10 "")))


(defmacro with-open-fstream ((f fn &key (dir "/dev/shm")) &body body)
  `(let ((,f :type "std::ofstream" :ctor (comma-list (string ,(format nil "~a/~a" dir fn)))))
     ,@body))

(let*
    ((code
      `(with-compilation-unit
	   (with-compilation-unit
	       (raw "//! \\file main.cpp "))
	 (raw "// compile gradient-halide with")
	 (raw "// pacman -S openblas eigen")
	 (raw "// export HL_DEBUG_CODEGEN=1; export HL_TARGET=host-opencl;export HL_JIT_TARGET=host-opencl; cmake -DCMAKE_BUILD_TYPE=Release -DOpenGL_GL_PREFERENCE=GLVND ..")
	 (raw "// g++ main.cpp -g -std=c++11 -I /usr/local/share/halide -I /usr/local/share/halide/tools -I /usr/local/share/halide/tutorial -I /usr/local/include -L /usr/local/lib -lHalide `libpng-config --cflags --ldflags` -ljpeg -lpthread -ldl -o main -Wl,-rpath=/usr/local/lib")
	 (include "Halide.h")

	 (raw "using namespace Halide;")

	 
	 (raw " ")
	 (raw " ")
	 (raw "//! \\mainpage test halide")
	 (raw "//! \\section Introduction")
	 (raw "//! this is a test of the gradient halide code generator")
	 (raw "//! \\section Dependencies ")
	 (raw "//! - Halide ")
	 (raw " ")
	 (raw "//! - cxxopts for commandline argument parsing.")
	 (raw " ")
	 (raw "//! - sbcl to generate c++ code")
	 (raw "//! - g++ to compile c++ code")
	 (raw " ")
	 (raw "//! - For the documentation (optional for use):")
	 (raw "//!   + doxygen")
	 (raw " ")
	 (raw " ")
	 (raw "//! \\section References ")
	 ,@(loop for i from 1 and e in
		'("https://github.com/halide/Halide/blob/5d298f0d1731e5b89ee6ff1ae948a5201e6ea471/test/correctness/fit_function.cpp"
		  "https://github.com/halide/Halide/wiki/Debugging-Tips")
	      collect
		`(raw ,(format nil "//! ~a. ~a" i e)))

	 
	 ,@(dox :brief "main function"
		:usage ""
		:params '((argc "input number of command line arguments")
			  (argv "input"))
		:return "Integer")
	 
	 (function (main ((argc :type int)
			  (argv :type char**)) int)
		   (let ((coeffs :type ImageParam
				 :ctor (comma-list
					(funcall Float 64)
					1))
			 (learning_rate :type Param<double>)
			 (order :type Param<int>)
			 (samples :type Param<int>)
			 (approx_sin :type Func)
			 
			 (x :type Var)
			 (y :type Var)
			 (fx :type Expr
			     :init
			     (*
			      (/ x
				 (funcall cast<double>
					  samples))
			      (funcall Expr
				       #.(/ pi 2))))
			 (r :type RDom :ctor (comma-list 0 order))
			 (r_flipped :type Expr
				    :init (- 1 r))
			 (exact_sin :type Func))
		     (setf (funcall approx_sin x y)
			   (funcall cast<double> 0.0)
			   (funcall approx_sin x r_flipped)
			   (+
			    (* fx (funcall coeffs r_flipped))
			    (* (funcall approx_sin
					x
					(+ 1 r_flipped))
			       fx))
			   (funcall exact_sin x)
			   (funcall sin fx))
		     (let ((err :type Func))
		       (setf (funcall err x)
			     (funcall pow
				      (/ (-
					  (funcall approx_sin x 0)
					  (funcall exact_sin x))
					 (funcall exact_sin x))
				      2)
			     )
		       (let ((d :type RDom
				:ctor (comma-list 1 (- samples 1)))
			     (avg_err :type Func))
			 (setf (funcall avg_err)
			       (/ (funcall sum
					   (funcall err d))
				  samples))
			 (let ((d_err_d :init
				 (funcall propagate_adjoints
					  avg_err))
			       (new_coeffs :type Func))
			   (setf (funcall new_coeffs x)
				 (- (funcall coeffs x)
				    (* learning_rate
				       (funcall 
					(funcall d_err_d coeffs)
					x))
				    ))
			   (slot-value
			    err
			    (funcall compute_root)
			    (funcall vectorize x 4))
			   (slot-value
			    new_coeffs
			    (funcall compute_root)
			    (funcall vectorize x 4))
			   (slot-value
			    approx_sin
			    (funcall compute_root)
			    (funcall vectorize x 4)
			    (funcall update)
			    (funcall vectorize x 4))
			   (slot-value
			    avg_err
			    (funcall compute_root))
			   (let ((v :type Var)
				 ((aref fs) :type Func
				  :init (list coeffs
					      approx_sin
					      err)))
			     (for-range (f fs)
					(let ((first :type bool
						     :init true))
					  (for-range (df
						      (funcall
						       d_err_d.funcs f))
						     (if first
							 (statements
							  (setf first false)
							  (raw continue)))
						     (slot-value
						      df
						      (funcall compute_root)
						      (funcall vectorize
							       (aref (funcall df.args)  0) 4))
						     (dotimes (i (funcall df.num_update_definitions))
						       (for-range (d
								   (slot-value df
									       (funcall update i)
									       (funcall get_schedule)
									       (funcall dims)))
								  (if (funcall d.is_pure)
								      (statements
								       (slot-value df
										   (funcall update i)
										   (funcall vectorize (funcall Var d.var)
											    4))
								       (raw break))))))))
			     (let ((terms :type "const int" :init 8)
				   (c :type Buffer<double> :ctor terms))
			       (funcall order.set terms)
			       (funcall samples.set 1000)
			       (let ((e :init (funcall "Buffer<double>::make_scalar")))
				 (funcall coeffs.set c)
				 (let ((p :type Pipeline :ctor (list avg_err new_coeffs)))
				   (setf (funcall c 0) 1)
				   (dotimes (i terms)
				     (setf (funcall c i)
					   (/ (- (funcall c (- i 1)))
					      (* i 2 (+ 1 (* i 2))))))
				   (let ((steps :type "const int" :init 10000)
					 (initial_error :type double :init 0d0))
				     (funcall learning_rate.set .00001)
				     (dotimes (i steps)
				       (let ((should_print :type bool
							   :init
							   (|\|\||
							    (|\|\||
							     (== 0 i)
							     (== (/ steps 2) i)
							     )
							    (== steps i))))
					 (if should_print
					     (statements
					      (funcall printf
						       (string "Iteration %d\\nCoefficients: ")
						       i)
					      (dotimes (j terms)
						(funcall printf (string "%g ")
							 (funcall c j)))
					      (funcall printf (string "\\n"))))
					 (funcall p.realize (list e c))
					 (if should_print
					     (funcall printf (string "Error: %g\\n") (funcall e)))
					 (if (== 0 i)
					     (setf initial_error (funcall e))))))))
			       )
			     
			     )))))))))
  (write-source "stage/cl-gen-halide-test/source/main" "cpp" code))


                                
