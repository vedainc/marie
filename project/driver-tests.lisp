;;;; driver-tests.lisp --- symbol driver tests

(uiop:define-package :${project}/t/driver-tests
  (:nicknames #:${project}/t)
  (:use #:uiop/common-lisp
        #:marie)
  (:use-reexport #:${project}/t/core-tests))

(provide "${project}/t")
(provide "${PROJECT}/T")
