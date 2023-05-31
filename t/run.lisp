;;;; -*- mode: lisp; syntax: common-lisp; base: 10; coding: utf-8-unix; external-format: (:utf-8 :eol-style :lf); -*-
;;;; tests.lisp: main file for the unit tests

(uiop:define-package #:marie/t/run
    (:use #:cl
          #:fiveam
          #:marie))

(in-package #:marie/t/run)


;;; entrypoint

(def run-tests ()
  "Run all the tests defined in the suite."
  (run-all-tests))
