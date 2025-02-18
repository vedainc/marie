;;;; -*- mode: lisp; syntax: common-lisp; base: 10; coding: utf-8-unix; external-format: (:utf-8 :eol-style :lf); -*-
;;;; driver.lisp --- top-level definitions for exporting marie symbols

(uiop:define-package #:marie/src/driver
  (:nicknames :marie :m)
  (:use :uiop/common-lisp)
  (:use-reexport #:marie/src/definitions
                 #:marie/src/reader
                 #:marie/src/sequences
                 #:marie/src/symbols
                 #:marie/src/conditionals
                 #:marie/src/strings
                 #:marie/src/hash
                 #:marie/src/etc
                 #:marie/src/filesystem
                 #:marie/src/system
                 #:marie/src/project))

(provide "marie")
(provide "MARIE")
