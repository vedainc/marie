;;;; -*- mode: lisp; syntax: common-lisp; base: 10; -*-
;;;; main.lisp --- main entry point

(uiop:define-package #:${project}/src/main
  (:use #:cl
        #:marie
        #:${project}/src/specials
        #:${project}/src/core
        #:${project}/src/key
        #:${project}/src/etc))

(in-package #:${project}/src/main)


;;; entry point

(define-main-command
    (("verbose" "verbosity" :counter 0))
    (key
     search
     date
     zsh-completions
     print-doc))

(define-main)
