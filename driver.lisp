;;;; -*- mode: lisp; syntax: common-lisp; base: 10; coding: utf-8-unix; external-format: (:utf-8 :eol-style :lf); -*-
;;;; driver.lisp: top-level definitions for exporting marie symbols

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
