;;;; driver.lisp - re-export all the functionality in MARIE

(uiop:define-package :marie/driver
  (:nicknames :marie)
  (:use :uiop/common-lisp)
  (:use-reexport #:marie/strings
                 #:marie/sequences
                 #:marie/symbols
                 #:marie/files
                 #:marie/reader
                 #:marie/etc))

(provide "marie")
(provide "MARIE")
