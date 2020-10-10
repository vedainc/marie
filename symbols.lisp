;;;; symbols.lisp

(uiop:define-package #:marie/symbols
  (:use #:cl
        #:marie/defs))

(in-package #:marie/symbols)

(defm symbols (package &optional (location :symbols))
  "Return the symbols in PACKAGE denoted by LOCATION."
  `(loop :for symbol
         :being :the ,location
         :in (find-package ,package)
         :collect symbol))

(def external-symbols (package)
  "Return the external symbols in PACKAGE."
  (symbols package :external-symbols))

(def present-symbols (package)
  "Return the external symbols in PACKAGE."
  (symbols package :present-symbols))

(defm with-gensyms ((&rest names) &body body)
  "Evaluate BODY where NAMES are unique symbols."
  `(let ,(loop :for napme :in names :collect `(,name (gensym)))
     ,@body))

(def symbol* (value)
  "Return a symbol from VALUE."
  (etypecase value
    (number value)
    (string (intern (string-upcase value)))
    (t value)))

(defm mx (form)
  "Pretty print the macro expansion of FORM."
  `(let* ((text "MACROEXPAND")
          (value-1 (macroexpand-1 ,form))
          (value-2 (macroexpand ,form)))
     (cond ((equal value-1 value-2)
            (format t "~&~A:~%~S" text value-1))
           (t (format t "~&~A-1:~%~S" text value-1)
              (format t "~&~A:~%~S" text value-2)))
     (values)))

(defm mapply (macro &rest args)
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

(defm rename (name-1 name-2)
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

(defm swap (name-1 name-2)
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
