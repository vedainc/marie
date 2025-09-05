;;;; -*- mode: lisp; syntax: common-lisp; base: 10; coding: utf-8-unix; external-format: (:utf-8 :eol-style :lf); -*-
;;;; declarations.lisp --- declare, declaim, proclaim, etc

(uiop:define-package #:marie/src/declarations
  (:use #:cl
        #:marie/src/definitions
        #:marie/src/strings
        #:marie/src/etc))

(in-package #:marie/src/declarations)

(defm ftype (name args type)
  "Return a DECLAIM for FTYPE."
  `(declaim (ftype (function ,args) ,type) ,name))

(defm vtype (name type)
  "Return a DECLAIM for TYPE."
  `(declaim (type ,type ,name)))

(defm optimize (&rest rest)
  "Return a DECLARE for OPTIMIZE."
  `(declare (optimize ,@rest)))
