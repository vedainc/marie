;;;; core-tests.lisp -- core functions tests

(uiop:define-package #:${project}/t/core-tests
  (:use #:cl #:marie
        #:fiveam
        #:${project}))

(in-package #:${project}/t/core-tests)

(def run-tests ()
  "Run all the tests defined in the suite."
  (run-all-tests))
