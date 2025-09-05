;;;; -*- mode: lisp; syntax: common-lisp; base: 10 -*-
;;;; marie.asd --- main ASDF file of marie

(defsystem #:marie
    :name "marie"
    :description "A tiny collection of CL utilities without external dependencies"
    :version #.(uiop:read-file-form (make-pathname :directory '(:relative "src") :name "version" :type "lisp"))
    :author "Rommel Martinez <ebzzry@icloud.com>"
    :licence "BSD-3-Clause"
    :class :package-inferred-system
    :depends-on (#+sbcl #:sb-cltl2
                 #+lispworks #:marie/src/lispworks
                 #:marie/src/definitions
                 #:marie/src/declarations
                 #:marie/src/reader
                 #:marie/src/symbols
                 #:marie/src/conditionals
                 #:marie/src/sequences
                 #:marie/src/strings
                 #:marie/src/hash
                 #:marie/src/etc
                 #:marie/src/filesystem
                 #:marie/src/system
                 #:marie/src/project
                 #:marie/src/init
                 #:marie/src/driver)
    :in-order-to ((test-op (test-op "marie-tests"))))
