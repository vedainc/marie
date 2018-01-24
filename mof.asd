;;;; mof.asd

(defpackage :mof-system
  (:use #:cl #:asdf))

(in-package :mof-system)

(defsystem :mof
  :name "mof"
  :version "0.5"
  :author "Rommel Martinez <ebzzry@ebzzry.io>"
  :license "BSD License"
  :description "Miscellaneous utilities"
  :depends-on (#:cl-who
               #:ironclad
               #+SBCL
               #:sb-posix)
  :components ((:module
                "src"
                :serial t
                :components ((:file "packages")
                             (:file "lists")
                             (:file "strings")
                             (:file "symbols")
                             (:file "misc")
                             (:file "collect")
                             (:file "matrix")))))

;;; 2007-10-02
