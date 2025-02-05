;;;; user-tests.lisp --- user sandbox tests

(uiop:define-package :${project}/t/user-tests
  (:nicknames #:${project}-tests-user)
  (:use #:cl #:marie
        #:${project}/t/driver-tests))

(in-package #:${project}-tests-user)
