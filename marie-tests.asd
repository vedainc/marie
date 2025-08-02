;;;; -*- mode: lisp; syntax: common-lisp; base: 10; coding: utf-8-unix; external-format: (:utf-8 :eol-style :lf); -*-
;;;; marie-tests.asd --- test ASDF file of marie

(defsystem #:marie-tests
    :name "marie-tests"
    :version #.(uiop:read-file-form (make-pathname :directory '(:relative "t") :name "version" :type "lisp"))
    :description "ASDF test file of marie"
    :class :package-inferred-system
    :depends-on (#:fiveam
                 #:marie
                 #:marie/t/run
                 #:marie/t/driver)
    :perform (test-op (o c) (uiop:symbol-call :marie/t/run :run-tests)))
