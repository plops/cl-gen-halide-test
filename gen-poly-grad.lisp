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

(progn
  (let* ((code `(with-compilation-unit
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
				  (exact_sin :type Func
					     :init (funcall sin fx)))
			      (setf (funcall approx_sin x y)
				    (funcall cast<double> 0.0)
				    (funcall approx_sin x r_flipped)
				    (+
				     (* fx (funcall coeffs r_flipped))
				     (* (funcall approx_sin
						 x
						 (+ 1 r_flipped))
					fx))
				    )
			      )))))
    (write-source "stage/cl-gen-halide-test/source/main" "cpp" code)))


                                
