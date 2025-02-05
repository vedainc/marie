;;;; driver.lisp --- symbol driver

(uiop:define-package #:${project}/src/driver
  (:nicknames #:${project})
  (:use #:uiop/common-lisp)
  (:use-reexport #:${project}/src/core))

(provide "${project}")
(provide "${PROJECT}")
