;;;; -*- mode: lisp; syntax: common-lisp; base: 10; -*-
;;;; key.lisp --- generate and convert keys

(uiop:define-package #:${project}/src/key
  (:use #:cl
        #:marie
        #:${project}/src/core))

(in-package #:${project}/src/key)

(define-command key convert (c)
  "generate a public key for verifying paths"
  "<key>"
  nil
  t
  nil
  "Convert a secret key to a public key"
  "k c foo")

(define-command key generate (g)
  "generate a secret key for signing paths"
  ""
  nil
  t
  nil
  "Generate a new key"
  "k g")

(define-command nil key (k)
  "generate and convert keys"
  "<command>"
  nil
  #'print-usage
  (convert generate))
