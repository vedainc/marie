;;;; driver.lisp

(uiop:define-package :marie/driver
  (:nicknames :marie)
  (:use :uiop/common-lisp)
  (:use-reexport #:marie/defs
                 #:marie/strings
                 #:marie/sequences
                 #:marie/symbols
                 #:marie/files
                 #:marie/reader
                 #:marie/etc))

(provide "marie")
(provide "MARIE")
