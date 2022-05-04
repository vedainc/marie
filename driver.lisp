;;;; driver.lisp

(uiop:define-package :marie/driver
  (:nicknames :marie :m)
  (:use :uiop/common-lisp)
  (:use-reexport #:marie/defs
                 #:marie/reader
                 #:marie/sequences
                 #:marie/symbols
                 #:marie/files
                 #:marie/conditionals
                 #:marie/strings
                 #:marie/etc))

(provide "marie")
(provide "MARIE")
