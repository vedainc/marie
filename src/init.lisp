;;;; -*- mode: lisp; syntax: common-lisp; base: 10; -*-
;;;; init.lisp --- initialize marie

(uiop:define-package #:marie/src/init
  (:use #:cl
        #:marie/src/definitions
        #:marie/src/conditionals
        #:marie/src/reader))

(in-package #:marie/src/init)

;;; Oh, I’m going to get a lot of flak with these.
(use-bracket-reader)
(use-lambda-reader)

;; (use-alpha-reader)
;; (use-phi-reader)
