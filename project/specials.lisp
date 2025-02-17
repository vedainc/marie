;;;; specials.lisp --- special variables

(uiop:define-package #:${project}/src/specials
  (:use #:cl
        #:marie))

(in-package #:${project}/src/specials)


;;; entry point

(defk +project-name+
  "${project}"
  "The name of the project.")

(defk +project-version+
  "0.0.0"
  "The version number of the project.")

(defk +project-description+
  "A program for blah blah blah"
  "The description of the project.")

(defk +exe+
  '("echo")
  "The name of the main command line program.")
