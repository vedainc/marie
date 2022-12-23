;;;; -*- mode: lisp; syntax: common-lisp; base: 10; coding: utf-8-unix; external-format: (:utf-8 :eol-style :lf); -*-
;;;; reader.lisp: minor tweaks to the lisp reader

(uiop:define-package #:marie/reader
  (:use #:cl
        #:marie/definitions))

(in-package #:marie/reader)

(def- bracket-reader (stream char)
  "Use [+ _ 1] as a shorthand for #'(lambda (_) (+ _ 1))
See http://www.bradediger.com/blog/2008/03/stealing_from_arc.html"
  (declare (ignore char))
  `(lambda (,(intern "_") &optional ,(intern "__"))
     (declare (ignorable ,(intern "__")))
     ,(read-delimited-list #\] stream t)))

(def use-bracket-reader ()
  "Put the bracket reader into effect."
  (set-macro-character #\[ #'bracket-reader)
  (set-macro-character #\] (get-macro-character #\) nil)))

(def- brace-reader (stream char)
  "Use {foo 5} as a shorthand for (funcall foo 5)
See http://dorophone.blogspot.com/2008/03/common-lisp-reader-macros-simple.html"
  (declare (ignore char))
  `(funcall ,@(read-delimited-list #\} stream t)))

(def use-brace-reader ()
  "Put the brace reader into effect."
  (set-macro-character #\{ #'brace-reader)
  (set-macro-character #\} (get-macro-character #\) nil)))

(defm with-preserved-case ((&optional) &body body)
  "Evaluate BODY while preserving the read case."
  `(let ((*readtable* (copy-readtable nil)))
     (setf (readtable-case *readtable*) :preserve)
     (progn ,@body)))

(def read-from-string* (string)
  "Evaluate STRING with preserved case."
  (with-preserved-case ()
    (read-from-string string)))

(def- lambda-reader (stream char)
  "Define the reader for λ."
  (declare (ignore stream char))
  'LAMBDA)

(set-macro-character #\λ #'lambda-reader)

(def- dollar-reader (stream char)
  (declare (ignore char))
  (list (quote function) (read stream t nil t)))

(def use-dollar-reader ()
  "Put the dollar reader into effect."
  (set-macro-character #\$ #'dollar-reader))
