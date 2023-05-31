;;;; -*- mode: lisp; syntax: common-lisp; base: 10; coding: utf-8-unix; external-format: (:utf-8 :eol-style :lf); -*-
;;;; driver.lisp: top-level definitions for the tests

(uiop:define-package #:marie/t/driver
    (:nicknames #:marie/t)
  (:use #:uiop/common-lisp
        #:marie)
  (:use-reexport #:marie/t/run))

(provide "marie/t")
(provide "MARIE/T")
