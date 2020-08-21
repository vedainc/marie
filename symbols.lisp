;;;; symbols.lisp

(uiop:define-package #:marie/symbols
  (:use #:cl)
  (:export #:def
           #:defm
           #:defv
           #:defp
           #:defc
           #:symbols
           #:external-symbols
           #:present-symbols
           #:symbol*
           #:with-gensyms
           #:mx
           #:mapply
           #:unbind
           #:rename
           #:swap
           #:flet*))

(in-package #:marie/symbols)

(defun cexport (symbol)
  "Export SYMBOL only if it matches a certain criteria."
  (let ((name (string symbol)))
    (unless (and (not (zerop (length name)))
                 (member (elt name 0) '(#\%) :test #'equal))
      (export symbol))))

(defmacro def (spec args &rest body)
  "Define a function with aliases and export the names. SPEC is either a single symbol, or a list
where the first element is the name of the function and the rest are aliases."
  (destructuring-bind (name &rest aliases)
      (uiop:ensure-list spec)
    `(progn
       (defun ,name ,args ,@body)
       (cexport ',name)
       ,@(loop :for alias :in aliases
               :when alias
               :collect `(progn (setf (fdefinition ',alias) (fdefinition ',name))
                                (export ',alias))))))

(defmacro defm (spec &rest body)
  "Define a macro with aliases and export the names. SPEC is either a single symbol, or a list where
the first element is the name of the function and the rest are aliases."
  (destructuring-bind (name &rest aliases)
      (uiop:ensure-list spec)
    `(progn
       (defmacro ,name ,@body)
       (cexport ',name)
       ,@(loop :for alias :in aliases
               :when alias
               :collect `(progn (setf (macro-function ',alias) (macro-function ',name))
                                (export ',alias))))))

(defmacro defv (spec &rest body)
  "Define a special variable by DEFVAR with aliases and export the names. SPEC is either a single
symbol, or a list where the first element is the name of the function and the rest are aliases."
  (destructuring-bind (name &rest aliases)
      (uiop:ensure-list spec)
    `(progn
       (defvar ,name ,@body)
       (cexport ',name)
       ,@(loop :for alias :in aliases
               :when alias
               :collect `(progn (defvar ,alias ,@body)
                                (export ',alias))))))

(defmacro defp (spec &rest body)
  "Define a special variable by DEFPARAMETER and export the names. SPEC is either a single symbol,
or a list where the first element is the name of the function and the rest are aliases."
  (destructuring-bind (name &rest aliases)
      (uiop:ensure-list spec)
    `(progn
       (defparameter ,name ,@body)
       (cexport ',name)
       ,@(loop :for alias :in aliases
               :when alias
               :collect `(progn (defparameter ,alias ,@body)
                                (export ',alias))))))

(defun call-continue-restart (condition)
  "Call the continue restart on CONDITION."
  (let ((restart (find-restart 'continue condition)))
    (when restart
      (invoke-restart restart))))

(defmacro defc (spec &rest body)
  "Bind NAME to VALUE and only change the binding after subsequent calls to the macro."
  (let ((id (if (consp spec) spec (list spec))))
    (destructuring-bind (name &rest aliases)
        id
      `(handler-bind #+sbcl ((sb-ext:defconstant-uneql #'continue))
                     #-sbcl ((simple-error #'continue))
         (defconstant ,name ,@body)
         (export ',name)
         ,@(loop :for alias :in aliases
                 :when alias
                 :collect `(progn (defconstant ,alias ,@body)
                                  (export ',alias)))))))

(defmacro symbols (package &optional (location :symbols))
  "Return the symbols in PACKAGE denoted by LOCATION."
  `(loop :for symbol
         :being :the ,location
         :in (find-package ,package)
         :collect symbol))

(defun external-symbols (package)
  "Return the external symbols in PACKAGE."
  (symbols package :external-symbols))

(defun present-symbols (package)
  "Return the external symbols in PACKAGE."
  (symbols package :present-symbols))

(defmacro with-gensyms ((&rest names) &body body)
  "Evaluate BODY where NAMES are unique symbols."
  `(let ,(loop :for napme :in names :collect `(,name (gensym)))
     ,@body))

(defun symbol* (value)
  "Return a symbol from VALUE."
  (etypecase value
    (number value)
    (string (intern (string-upcase value)))
    (t value)))

(defmacro mx (form)
  "Pretty print the macro expansion of FORM."
  `(let* ((text "MACROEXPAND")
          (value-1 (macroexpand-1 ,form))
          (value-2 (macroexpand ,form)))
     (cond ((equal value-1 value-2)
            (format t "~&~S:~%~S" text value-1))
           (t (format t "~&~S-1:~%~S" text value-1)
              (format t "~&~S:~%~S" text value-2)))
     (values)))

(defmacro mapply (macro &rest args)
  "Invoke the macro MACRO to each item in ARGS."
  `(progn
     ,@(loop :for arg :in args :collect `(,macro ,arg))))

(defmacro unbind (arg-1 &optional arg-2)
  "Unbind ARG-1; if ARG-2 is present, unbind ARG-2 in instance of ARG-1."
  `(progn
     (when (fboundp ',arg-1)
       (fmakunbound ',arg-1))
     (when (boundp ',arg-1)
       (makunbound ',arg-1))
     (when (and ,arg-2 (slot-boundp ',arg-1 ',arg-2))
       (slot-makunbound ',arg-1 ',arg-2))
     (unintern ',arg-1)
     (values)))

(defmacro rename (name-1 name-2)
  "Rename the special variable NAME-1 to NAME-2."
  `(if (boundp ',name-1)
       (let ((value ,name-1))
         (defparameter ,name-2 value)
         (unbind ,name-1))
       (values)))

(defmacro swap (name-1 name-2)
  "Interchange the values of special variables NAME-1 and NAME-2."
  (let ((temp (gensym)))
    `(when (and (symbol-value ',name-1)
                (symbol-value ',name-2))
       (let ((,temp ,name-1))
         (setf (symbol-value ',name-1) ,name-2)
         (setf (symbol-value ',name-2) ,temp)
         (values)))))

(defmacro flet* (&rest body)
  "Evaluate BODY in LABELS."
  `(labels ,@body))
