(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload :cl-cpp-generator))

(in-package :cl-cpp-generator)

(push :cxxopts *features*)

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
		  (include "Halide.h")

		  
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
			 '("https://www.youtube.com/watch?v=dnFccCGvT90 Jonathan Ragan-Kelley at Microsoft Research (good discussion in the end)"
			   "https://www.youtube.com/watch?v=WvzlaRpmtVk Jonathan Ragan-Kelley CVPR Tutorial"
			   "https://www.youtube.com/watch?v=PTSVlT3Iq4U Dan Tull Live Coding"
			   "https://www.youtube.com/watch?v=3uiEyEKji0M Andrew Adams Introduction")
		       collect
			 `(raw ,(format nil "//! ~a. ~a" i e)))
		  
		  
		  ,@(dox :brief "main function"
			 :usage "parse command line parameters and draw to screen"
			 :params '((argc "input number of command line arguments")
				   (argv "input"))
			 :return "Integer")
		  		  
		  (function (main ((argc :type int)
				   (argv :type char**)) int)

			    
			    
			    
			    (with-compilation-unit 
				#+cxxopts (raw "try")
			      (let (#+cxxopts (options :type "cxxopts::Options" :ctor (comma-list (string "drm_draw") (string "draw to drm device"))))
				#+cxxopts
				(statements
				 (with-compilation-unit
				     (raw "options.add_options()")
				   (raw "(\"h,help\", \"Print help\")")
					; (raw "(\"d,device\",\"device file\",cxxopts::value<std::string>()->default_value(\"/dev/dri/card0\"))")
					; (raw "(\"r,rate\",\"frame rate (Hz,double)\",cxxopts::value<double>()->default_value(\"60.0\"))")
				   (raw ";"))
				  
				   (funcall options.parse argc argv)
				  
				   (if (funcall options.count (string "help"))
				       (statements
					(macroexpand (e (funcall options.help)))
					(funcall exit 0))))
				(let ((gradient :type "Halide::Func")
				      (x :type "Halide::Var")
				      (y :type "Halide::Var")
				      (e :type "Halide::Expr" :ctor (+ x y))
				      )
				  (setf (funcall gradient x y) e)
				  (let ((output :type "Halide::Buffer<int32_t>"
						:ctor (funcall gradient.realize 800 600)))
				    (for ((j 0 :type int) (< j (funcall output.height)) j++)
				      (for ((i 0 :type int) (< i (funcall output.width)) i++)
					(if (!= (+ i j) (funcall output i j))
					    (statements
					     (macroexpand (er "error, expected " (+ i j)
							      " but result is " (funcall output i j)))))))))
				)
			      
			      #+cxxopts (raw "catch (const cxxopts::OptionException& e)")
			      #+cxxopts (let ()
				(macroexpand (e "error parsing options: " (funcall e.what)))
				(funcall exit 1)))
			    
			    (return 0)))))
    (write-source "stage/cl-gen-halide-test/source/main" "cpp" code)))


                                
