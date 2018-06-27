#-asdf3.1 (error "ASDF 3.1 or bust!")

(defpackage :mof-system
  (:use #:cl #:asdf))

(in-package #:mof-system)

(defsystem :mof
  :name "mof"
  :version "0.0.7"
  :description "Miscellaneous utilities"
  :license "CC0"
  :author "Rommel Martinez <ebzzry@ebzzry.io>"
  :depends-on (#:ironclad
               #+SBCL
               #:sb-posix)
  :serial t
  :components ((:file "packages")
               (:file "sequences")
               (:file "strings")
               (:file "symbols")
               (:file "misc")
               (:file "collect")
               (:file "files")
               (:file "matrix")))
