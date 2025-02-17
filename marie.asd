;;;; -*- mode: lisp; syntax: common-lisp; base: 10; coding: utf-8-unix; external-format: (:utf-8 :eol-style :lf); -*-
;;;; marie.asd --- main ASDF file of marie

(defsystem #:marie
    :name "marie"
    :version (:read-file-form #P"version.lisp")
    :description "A small collection of CL utilities without external dependencies"
    :author "Krei, Inc."
    :class :package-inferred-system
    :depends-on (#+sbcl #:sb-cltl2
                 #:marie/src/definitions
                 #:marie/src/reader
                 #:marie/src/symbols
                 #:marie/src/conditionals
                 #:marie/src/sequences
                 #:marie/src/strings
                 #:marie/src/hash
                 #:marie/src/etc
                 #:marie/src/filesystem
                 #:marie/src/project
                 #:marie/src/driver)
    :in-order-to ((test-op (test-op "marie-tests"))))
