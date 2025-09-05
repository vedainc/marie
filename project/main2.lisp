;;;; main.lisp --- main entry point

(uiop:define-package #:${project}/src/main
  (:use #:cl)
  (:export #:main))

(in-package #:${project}/src/main)


;;; main

(defun main ()
  (format t "Hello, world!~%"))
