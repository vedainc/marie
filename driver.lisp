;;;; driver.lisp
;;;; top-level definitions for exporting definitions under a single package

(uiop:define-package :marie/driver
  (:nicknames :marie)
  (:use :uiop/common-lisp)
  (:use-reexport #:marie/definitions
                 #:marie/reader
                 #:marie/sequences
                 #:marie/symbols
                 #:marie/conditionals
                 #:marie/strings
                 #:marie/etc
                 #:marie/files))

(provide "marie")
(provide "MARIE")
