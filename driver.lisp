;;;; driver.lisp

(uiop:define-package :marie/driver
  (:nicknames :marie :m)
  (:use :uiop/common-lisp)
  (:use-reexport #:marie/defs
                 #:marie/reader
                 #:marie/sequences
                 #:marie/strings
                 #:marie/symbols
                 #:marie/files
                 #:marie/conditionals
                 #:marie/etc))

(provide "marie")
(provide "MARIE")
