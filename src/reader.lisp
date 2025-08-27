;;;; -*- mode: lisp; syntax: common-lisp; base: 10; coding: utf-8-unix; external-format: (:utf-8 :eol-style :lf); -*-
;;;; reader.lisp --- minor tweaks to the lisp reader

(uiop:define-package #:marie/src/reader
  (:use #:cl
        #:marie/src/definitions))

(in-package #:marie/src/reader)


;;; Brace reader

;;; http://www.bradediger.com/blog/2008/03/stealing_from_arc.html
(def- brace-reader (stream char)
  "Use `{+ _ 1}` as shorthand for #'(lambda (_) (+ _ 1)),
reading from stream and ignoring CHAR."
  (declare (ignore char)
           (optimize (speed 3) (safety 0)))
  `(lambda (,(intern "_") &optional ,(intern "__"))
     (declare (ignorable ,(intern "__")))
     ,(read-delimited-list #\} stream t)))

(def use-brace-reader ()
  "Put the brace reader into effect."
  (set-macro-character #\{ #'brace-reader)
  (set-macro-character #\} (get-macro-character #\) nil)))


;;; Bracket reader

(def- bracket-reader (stream char)
  "Use `[foo 5]` as shorthand for `(funcall foo 5)`, reading from stream and
ignoring CHAR. "
  (declare (ignore char)
           (optimize (speed 3) (safety 0)))
  `(funcall ,@(read-delimited-list #\] stream t)))

(def use-bracket-reader ()
  "Put the bracket reader into effect."
  (set-macro-character #\[ #'bracket-reader)
  (set-macro-character #\] (get-macro-character #\) nil)))


;;; Lambda reader

(def- lambda-reader (stream char)
  "Define the reader for λ.
(λ …) as a shorthand for (lambda …),
reading from stream and ignoring stream char. "
  (declare (ignore stream char))
  'LAMBDA)

(def- use-lambda-reader ()
  "Put the lambda reader into effect."
  (set-macro-character #\λ #'lambda-reader))


;;; Phi reader

(def- phi-reader (stream char)
  "(φ …) as a shordhand for (progn …),
 reading from stream and ignoring stream char. "
  (declare (ignore stream char)
           (optimize (speed 3) (safety 0)))
  'PROGN)

(def- use-phi-reader ()
  "Put the phi reader into effect."
  (set-macro-character #\φ #'phi-reader))


;;; Dollar reader

(def- dollar-reader (stream char)
  "Define the reader for dollar."
  (declare (ignore char)
           (optimize (speed 3) (safety 0)))
  (list 'function (read stream t nil t)))

(def use-dollar-reader ()
  "Put the dollar reader into effect."
  (set-macro-character #\$ #'dollar-reader))


;;; Preserving case
;;; This prevents Common Lisp's default behavior of converting symbol names to uppercase.

(defm with-preserved-case ((&optional) &body body)
  "Evaluate BODY while preserving the read case."
  `(let ((*readtable* (copy-readtable nil)))
     (setf (readtable-case *readtable*) :preserve)
     (progn ,@body)))

(def read-from-string* (string)
  "Evaluate STRING with preserved case."
  (declare (type string)
           (type string string)
           (inline read-from-string))
  (with-preserved-case ()
    (read-from-string string)))
