;;;; core.lisp --- core functions

(uiop:define-package #:${project}/src/core
  (:use #:cl
        #:marie))

(in-package #:${project}/src/core)

(def main^hello ()
  "Display a greeting."
  (format t "Hello, world!"))
