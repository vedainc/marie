;;;; main-tests.lisp -- main functions tests

(uiop:define-package #:${project}/t/main-tests
  (:use #:cl #:marie
        #:fiveam
        #:${project}))

(in-package #:${project}/t/main-tests)

(def run-tests ()
  "Run all the tests defined in the suite."
  (run-all-tests))
