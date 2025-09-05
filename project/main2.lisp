;;;; main.lisp --- main entry point

(uiop:define-package #:${project}/src/main
  (:use #:cl)
  (:export #:main))

(in-package #:${project}/src/main)


;;; main

(defun main ()
  (format t "These aren't the droids you're looking for."))
