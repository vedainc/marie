;; Date created: <2008-03-18 11:19 rmm>

(defpackage :mof-system
  (:use :cl :asdf))

(in-package :mof-system)

(defsystem :mof
  :name "mof"
  :version "0.5"
  :author "Rommel M. Martinez"
  :license "BSD License"
  :description "Miscellaneous utilities"
  :depends-on (:cl-who
	       :asdf-install
	       :ironclad
               #+SBCL
	       :sb-posix)
  :components ((:module
		"src"
		:serial t
		:components ((:file "packages")
			     (:file "mof")))))



