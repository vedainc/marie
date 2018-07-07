#-asdf3.1 (error "ASDF 3.1 or bust!")

(defpackage :mof-system
  (:use #:cl #:asdf))

(in-package #:mof-system)

(defsystem :mof
  :name "mof"
  :version "0.0.8"
  :description "Miscellaneous utilities"
  :license "CC0"
  :author "Rommel MARTINEZ <ebzzry@ebzzry.io>"
  :depends-on (#:ironclad
               #+sbcl
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
