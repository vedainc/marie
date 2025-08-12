;;;; -*- mode: lisp; syntax: common-lisp; base: 10; coding: utf-8-unix; external-format: (:utf-8 :eol-style :lf); -*-
;;;; lispworks.lisp --- utilities for handling conditional expressions

(uiop:define-package #:marie/src/lispworks
  (:use #:cl
        #:marie/src/definitions))

(in-package #:marie/src/lispworks)


;;; Documentation

(defm describe2 (symbol)
  `(let ((lispworks:*describe-level* 2))
     (describe ,symbol)))

(defm describe3 (symbol)
  `(let ((lispworks:*describe-level* 3))
     (describe ,symbol)))
