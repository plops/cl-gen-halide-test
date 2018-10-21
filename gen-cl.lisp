(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload :cl-cpp-generator))

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

		  (include "halide_image_io.h")
		  (raw "using namespace Halide::Tools;")
		  (include "clock.h")
		  (include <dlfcn.h>)
		  #+cxxopts (include "cxxopts.hpp")
		  (include <iostream>)
		  (include <array>)
		  (raw " ")
		  
		  (raw " ")
		  (raw "//! \\mainpage test halide")
		  (raw "//! \\section Introduction")
		  (raw "//! this is a test of the halide code generator")
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
			 '("http://halide-lang.org/tutorials/tutorial_lesson_12_using_the_gpu.html"
			   "https://github.com/halide/Halide/wiki/Debugging-Tips")
		       collect
			 `(raw ,(format nil "//! ~a. ~a" i e)))

		  (decl ((x :type Var)
			 (y :type Var)
			 (z :type Var)
			 (c :type Var)
			 (i :type Var)
			 (ii :type Var)
			 (xo :type Var)
			 (yo :type Var)
			 (xi :type Var)
			 (yi :type Var))
			)
		  (class MyPipeline ()
			 (access-specifier public)
			 (decl ((lut :type Func)
			       (padded :type Func)
			       (p16 :type Func)
			       (sharpen :type Func)
			       (curved :type Func)
			       (input :type Buffer<uint8_t>)))
			 (function (MyPipeline ((in :type Buffer<uint8_t>))
					       nil
					       :ctor ((input in)))
				   (setf
				    (funcall lut i)
				    (funcall
				     cast<uint8_t>
				     (funcall
				      clamp
				      (* 255f0
					 (funcall pow
						  (/ (funcall cast<float> i) 255.0f0)
						  1.2f))
				      0f0
				      255f0)))
				   (setf (funcall padded x y c)
					 (funcall
					  input
					  (funcall
					   clamp x 0
					   (- (funcall input.width) 1))
					  (funcall
					   clamp y 0
					   (- (funcall input.height) 1))
					  c))
				   (setf (funcall p16 x y c)
					 (funcall
					  cast<uint16_t>
					  (funcall padded x y c)))
				   (setf (funcall sharpen x y c)
					 (-
					  (* 2 (funcall p16 x y c))
					  (/
					       (+ (funcall p16 (- x 1) y c)
						  (funcall p16 x (- y 1) c)
						  (funcall p16 (+ x 1) y c)
						  (funcall p16 x (+ y 1) c))
					       4)))
				   (setf (funcall curved x y c)
					 (funcall
					  lut
					  (funcall sharpen x y c))))
			 (function (schedule_for_cpu () void)
				   (funcall lut.compute_root)
				   
				   (let ((yo :type Var)
					 (yi :type Var))
				     (statements
				      (slot-value
				       curved
				       (funcall
					reorder c x y)
				       (funcall bound c 0 3)
				       (funcall unroll c)))
				     (slot-value
				      curved
				      (funcall split y yo yi 16)
				      (funcall parallel yo))
				     (slot-value
				      sharpen
				      (funcall compute_at curved yi))
				     (funcall sharpen.vectorize x 8)
				     (slot-value
				      padded
				      (funcall store_at curved yo)
				      (funcall compute_at curved yi))
				     (slot-value
				      padded
				      (funcall vectorize x 16))
				     (funcall curved.compile_jit))
				   )
			 (function (schedule_for_gpu () void)
				   (funcall lut.compute_root)
				   (let ((block :type Var)
					 (thread :type Var))
				     (funcall lut.split i block thread 16)
				     (slot-value
				      lut
				      (funcall gpu_blocks block)
				      (funcall gpu_threads thread))
				     (slot-value
				      curved
				      (funcall reorder c x y)
				      (funcall bound c 0 3)
				      (funcall unroll c))
				     (funcall curved.gpu_tile
					      x y xo yo xi yi 8 8)
				     (funcall padded.compute_at
					      curved xo)
				     (funcall padded.gpu_threads x y)
				     (let ((target
					    :type Target
					    :init (funcall get_host_target)))
				       (funcall target.set_feature
						"Target::OpenCL")
				       #+nil  (funcall target.set_feature
						"Target::Debug")
				       )
				     (funcall curved.compile_jit)))
			 (function
			  (test_performance () void)
			  (let (((funcall output
					  (funcall input.width)
					  (funcall input.height)
					  (funcall input.channels)
					  ) :type Buffer<uint8_t>))
			    (funcall curved.realize output)
			    (let ((best_time :init 0d0))
			     (dotimes (i 3)
			       (let ((t1 :init (funcall current_time)))
				 (dotimes (j 100)
				   (funcall curved.realize output))
				 (funcall output.copy_to_host)
				 (let ((t2 :init (funcall current_time))
				       (elapsed :init (/ (- t2 t1) 100d0)))
				   (if (|\|\|| (== i 0)
					   (< elapsed best_time))
				       (setf best_time elapsed))
				   ))))))
			 )

		  (raw "bool have_opencl_or_metal();")
		  (function (load_opencl () bool)
			    (return (!= NULL
					(funcall dlopen
						 (string "libOpenCL.so")
						 RTLD_LAZY))))
		  ,@(dox :brief "main function"
			 :usage ""
			 :params '((argc "input number of command line arguments")
				   (argv "input"))
			 :return "Integer")
		  		  
		  (function (main ((argc :type int)
				   (argv :type char**)) int)
			    (if (! (funcall load_opencl))
				(funcall write 1 (string "error") 5))
			    (let ((input :type Buffer<uint8_t> :init
					 (funcall load_image
						  (string "images/rgb.png")))
				  (p2 :type MyPipeline
				      :ctor input))
			      (funcall p2.schedule_for_gpu)
			      (funcall p2.test_performance))
			    (return 0)))))
    (write-source "stage/cl-gen-halide-test/source/main" "cpp" code)))


                                
