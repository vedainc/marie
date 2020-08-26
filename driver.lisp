;;;; driver.lisp

(uiop:define-package :marie/driver
  (:nicknames :marie)
  (:use :uiop/common-lisp)
  (:use-reexport #:marie/defs
                 #:marie/reader
                 #:marie/strings
                 #:marie/sequences
                 #:marie/symbols
                 #:marie/files
                 #:marie/etc))

(provide "marie")
(provide "MARIE")
