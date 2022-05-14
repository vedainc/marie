;;;; driver.lisp

(uiop:define-package :marie/driver
  (:nicknames :marie)
  (:use :uiop/common-lisp)
  (:use-reexport #:marie/defs
                 #:marie/reader
                 #:marie/sequences
                 #:marie/symbols
                 #:marie/conditionals
                 #:marie/strings
                 #:marie/etc
                 #:marie/files))

(provide "marie")
(provide "MARIE")
