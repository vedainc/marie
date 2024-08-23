;;;; -*- mode: lisp; syntax: common-lisp; base: 10; coding: utf-8-unix; external-format: (:utf-8 :eol-style :lf); -*-
;;;; symbols.lisp --- utilities for dealing with symbols

(uiop:define-package #:marie/src/symbols
  (:use #:cl
        #:marie/src/definitions))

(in-package #:marie/src/symbols)

(def symbols (package &key (type :external-symbols) sort)
  "Return the symbols interned in PACKAGE by TYPE."
  (let ((symbols '()))
    (macrolet ((mac (fn)
                 `(,fn (symbol (find-package package))
                      (push symbol symbols))))
      (ecase type
        ((:symbols) (mac do-symbols))
        ((:external-symbols) (mac do-external-symbols))
        (null nil))
      (if sort
          (mapcar #'read-from-string (sort (mapcar #'string symbols) sort))
          symbols))))

(def symbols* (package &key (type :external-symbols) (sort #'string<))
  "Print the symbols interned in PACKAGE by TYPE."
  (let ((symbols (symbols package :type type :sort sort)))
    (format t "~{~A~%~}" symbols)))

(def external-symbols (package &key sort)
  "Return the external symbols in PACKAGE."
  (symbols package :type :external-symbols :sort sort))

(def present-symbols (package)
  "Return the present symbols in PACKAGE."
  (symbols package :present-symbols))

(def pretty-print-symbols^pps (package &optional (type :external-symbols) (sort #'string<))
  "Display the external symbols in PACKAGE in the order that they were declared as dependencies."
  (let ((dependencies (asdf:system-depends-on (asdf:find-system package))))
    (loop :for dependency :in dependencies
          :do (let* ((symbols (symbols (read-from-string dependency) :type type))
                     (sorted-symbols (sort symbols sort)))
                (format t "~&~%** ~A~%~{~A~^~%~}" dependency sorted-symbols)))))

(defm with-gensyms ((&rest names) &body body)
  "Evaluate BODY where NAMES are unique symbols."
  `(let ,(loop :for name :in names :collect `(,name (gensym)))
     ,@body))

(defm macro-expand^mx (form)
  "Pretty print the macro expansion of FORM."
  `(let* ((text "MACROEXPAND")
          (value-1 (macroexpand-1 ,form))
          (value-2 (macroexpand ,form)))
     (cond ((equal value-1 value-2)
            (format t "~&~A:~%~S" text value-1))
           (t (format t "~&~A-1:~%~S" text value-1)
              (format t "~&~A:~%~S" text value-2)))
     (values)))

(defm macro-apply^mapply (macro &rest args)
  "Invoke the macro MACRO to each item in ARGS."
  `(progn
     ,@(loop :for arg :in args :collect `(,macro ,arg))))

(defm free (arg-1 &optional arg-2)
  "Unbind ARG-1; if ARG-2 is present, free ARG-2 in instance of ARG-1."
  `(progn
     (when (fboundp ',arg-1)
       (fmakunbound ',arg-1))
     (when (boundp ',arg-1)
       (makunbound ',arg-1))
     (when (and ,arg-2 (slot-boundp ',arg-1 ',arg-2))
       (slot-makunbound ',arg-1 ',arg-2))
     (unintern ',arg-1)
     (values)))

(defm rename-special-variable (name-1 name-2)
  "Rename the special variable NAME-1 to NAME-2."
  `(if (boundp ',name-1)
       (let ((value-1 ,name-1)
             (genstring (string (gensym))))
         (cond ((not (equalp (defvar ,name-1 genstring) genstring))
                (defvar ,name-2 value-1))
               (t (defparameter ,name-2 value-1)))
         (free ,name-1)
         ',name-2)
       (values)))

(defm swap-special-variables (name-1 name-2)
  "Interchange the values of special variables NAME-1 and NAME-2."
  (let ((temp (gensym)))
    `(when (and (symbol-value ',name-1)
                (symbol-value ',name-2))
       (let ((,temp ,name-1))
         (setf (symbol-value ',name-1) ,name-2)
         (setf (symbol-value ',name-2) ,temp)
         (values)))))

(defm flet* (&rest body)
  "Evaluate BODY in LABELS."
  `(labels ,@body))

(defm unbind (symbol)
  "Remove the bindings of SYMBOL."
  `(progn
     (when (boundp ',symbol) (makunbound ',symbol))
     (when (fboundp ',symbol) (fmakunbound ',symbol))))

(defm rename-macro (name-1 name-2)
  "Rename macro from NAME-1 to NAME-2."
  (with-gensyms (definition)
    `(when (macro-function ',name-1)
       (let ((,definition (macro-function ',name-1)))
         (unbind ,name-1)
         (unbind ,name-2)
         (setf (macro-function ',name-2)
               ,definition)
         (values)))))
