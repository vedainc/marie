;;;; defs.lisp

(uiop:define-package #:marie/defs
  (:use #:cl)
  (:export #:def-
           #:def
           #:defm-
           #:defm
           #:defv-
           #:defv
           #:defp-
           #:defp
           #:defk-
           #:defk
           #:defg-
           #:defg
           #:deft-
           #:deft
           #:defc))

(in-package #:marie/defs)

(defmacro %def (names args &rest body)
  "Define a function with aliases, and conditionally export the names.

NAMES is either a single symbol, or a list of symbols where the first element is the name of the function and the rest are aliases. Export the symbols if T is present in NAMES."
  (destructuring-bind (name &rest aliases)
      (uiop:ensure-list names)
    `(progn
       (defun ,name ,args ,@body)
       ,@(loop :for alias :in (remove t aliases)
               :collect `(setf (fdefinition ',alias) (fdefinition ',name)))
       (when (member t ',aliases)
         (progn
           (export ',name)
           ,@(loop :for alias :in (remove t aliases)
                   :collect `(export ',alias)))))))

(defmacro def- (names args &rest body)
  "Define a function wit %DEF but do not export NAMES."
  `(%def ,names ,args ,@body))

(defmacro def (names args &rest body)
  "Define a function with %DEF then export NAMES."
  `(%def ,(append (uiop:ensure-list names) (list t)) ,args ,@body))

(defmacro %defm (names args &rest body)
  "Define a macro with aliases, and conditionally export the names.

NAMES is either a single symbol, or a list of symbols where the first element is the name of the function and the rest are aliases. Export the symbols if T is present in NAMES."
  (destructuring-bind (name &rest aliases)
      (uiop:ensure-list names)
    `(progn
       (defmacro ,name ,args ,@body)
       ,@(loop :for alias :in (remove t aliases)
               :collect `(setf (macro-function ',alias) (macro-function ',name)))
       (when (member t ',aliases)
         (progn
           (export ',name)
           ,@(loop :for alias :in (remove t aliases)
                   :collect `(export ',alias)))))))

(defmacro defm- (names args &rest body)
  "Define a macro wit %DEFM but do not export NAMES."
  `(%defm ,names ,args ,@body))

(defmacro defm (names args &rest body)
  "Define a macro wit %DEFM then export NAMES."
  `(%defm ,(append (uiop:ensure-list names) (list t)) ,args ,@body))

(defmacro %defv (names &rest body)
  "Define special variables with DEFVAR, and conditionally export the names.

NAMES is either a single symbol, or a list of symbols where the first element is the name of the function and the rest are aliases. Export the symbols if T is present in NAMES."
  (destructuring-bind (name &rest aliases)
      (uiop:ensure-list names)
    `(progn
       (defvar ,name ,@body)
       ,@(loop :for alias :in (remove t aliases)
               :collect `(defvar ,alias ,@body))
       (when (member t ',aliases)
         (progn
           (export ',name)
           ,@(loop :for alias :in (remove t aliases)
                   :collect `(export ',alias)))))))

(defmacro defv- (names &rest body)
  "Define a special variable with %DEFV but do not export NAMES."
  `(%defv ,names ,@body))

(defmacro defv (names &rest body)
  "Define a special variable wit %DEFV then export NAMES."
  `(%defv ,(append (uiop:ensure-list names) (list t)) ,@body))

(defmacro %defp (names &rest body)
  "Define special variables with DEFPARAMETER, and conditionally export the names.

NAMES is either a single symbol, or a list of symbols where the first element is the name of the function and the rest are aliases. Export the symbols if T is present in NAMES."
  (destructuring-bind (name &rest aliases)
      (uiop:ensure-list names)
    `(progn
       (defparameter ,name ,@body)
       ,@(loop :for alias :in (remove t aliases)
               :collect `(defparameter ,alias ,@body))
       (when (member t ',aliases)
         (progn
           (export ',name)
           ,@(loop :for alias :in (remove t aliases)
                   :collect `(export ',alias)))))))

(defmacro defp- (names &rest body)
  "Define a special variable wit %DEFP but do not export NAMES."
  `(%defp ,names ,@body))

(defmacro defp (names &rest body)
  "Define a special variable with %DEFP then export NAMES."
  `(%defp ,(append (uiop:ensure-list names) (list t)) ,@body))

(defmacro %defk (names &rest body)
  "Define constants with DEFCONSTANT but allow the definition to change on subsequent calls to %DEFK,, and conditionally export the names.

NAMES is either a single symbol, or a list of symbols where the first element is the name of the function and the rest are aliases. Export the symbols if T is present in NAMES."
  (let ((id (if (consp names) names (list names))))
    (destructuring-bind (name &rest aliases)
        id
      `(handler-bind #+sbcl ((sb-ext:defconstant-uneql #'continue))
                     #-sbcl ((simple-error #'continue))
         (defconstant ,name ,@body)
         ,@(loop :for alias :in (remove t aliases)
                 :collect `(defconstant ,alias ,@body))
         (when (member t ',aliases)
           (progn
             (export ',name)
             ,@(loop :for alias :in (remove t aliases)
                     :collect `(export ',alias))))))))

(defmacro defk- (names &rest body)
  "Define a special variable with %DEFK but do not export NAMES."
  `(%defk ,names ,@body))

(defmacro defk (names &rest body)
  "Define a special variable with %DEFK then export NAMES."
  `(%defk ,(append (uiop:ensure-list names) (list t)) ,@body))

(defmacro %defg (names (&rest parameters) &body body)
  "Define a generic function with DEFGENERIC with aliases, and conditionally export the names.

NAMES is either a single symbol, or a list of symbols where the first element is the name of the function and the rest are aliases. Export the symbols if T is present in NAMES."
  (destructuring-bind (name &rest aliases)
      (uiop:ensure-list names)
    `(progn
       (defgeneric ,name (,@parameters) ,@body)
       ,@(loop :for alias :in (remove t aliases)
               :collect `(defgeneric ,alias (,@parameters) ,@body))
       (when (member t ',aliases)
         (progn
           (export ',name)
           ,@(loop :for alias :in (remove t aliases)
                   :collect `(export ',alias)))))))

(defmacro defg- (names (&rest parameters) &rest body)
  "Define a generic function with %DEFG but do not export NAMES."
  `(%defg ,names ,parameters ,@body))

(defmacro defg (names (&rest parameters) &rest body)
  "Define a generic function with %DEFG then export NAMES."
  `(%defg ,(append (uiop:ensure-list names) (list t)) ,parameters ,@body))

(defmacro %deft (names (&rest parameters) &body body)
  "Define a method with DEFMETHOD with aliases, and conditionally export the names.

NAMES is either a single symbol, or a list of symbols where the first element is the name of the function and the rest are aliases. Export the symbols if T is present in NAMES."
  (destructuring-bind (name &rest aliases)
      (uiop:ensure-list names)
    `(progn
       (defmethod ,name (,@parameters) ,@body)
       ,@(loop :for alias :in (remove t aliases)
               :collect `(defmethod ,alias (,@parameters) ,@body))
       (when (member t ',aliases)
         (progn
           (export ',name)
           ,@(loop :for alias :in (remove t aliases)
                   :collect `(export ',alias)))))))

(defmacro deft- (names (&rest parameters) &rest body)
  "Define a generic function with %DEFT but do not export NAMES."
  `(%deft ,names ,parameters ,@body))

(defmacro deft (names (&rest parameters) &rest body)
  "Define a generic function with %DEFT then export NAMES."
  `(%deft ,(append (uiop:ensure-list names) (list t)) ,parameters ,@body))

(defun p-symbol (symbol)
  "Return a conditionally hyphenated predicate symbol."
  (let* ((string (prin1-to-string symbol))
         (split (uiop:split-string string :separator '(#\-))))
    (if (> (length split) 1)
        (read-from-string (format nil "~{~A~^-~}" (append split '("P"))))
        (read-from-string (format nil "~{~A~^-~}P" split)))))

(defmacro defc (name (&rest superclasses) (&rest slot-specs)
                &optional class-option)
  "Define a class with DEFCLASS and export the slots and the class name."
  (flet ((fn (predicate &rest names)
             (let ((val (read-from-string
                         (format nil "~{~A~^-~}"
                                 (mapcar (lambda (name)
                                           (string-upcase (string name)))
                                         names)))))
               (if predicate
                   (p-symbol val)
                   val))))
    (let ((exports (mapcan (lambda (spec)
                             (let ((name (or (getf (cdr spec) :accessor)
                                             (getf (cdr spec) :reader)
                                             (getf (cdr spec) :writer))))
                               (when name (list name))))
                           slot-specs))
          (make-name (fn nil 'make name))
          (p-name (fn t name)))
      `(progn
         (defclass ,name (,@superclasses)
           ,@(append (list slot-specs)
              (when class-option
                (list class-option))))
         (defun ,make-name (&rest args)
           ,(format nil "Return a new instance of ~A." name)
           (apply #'make-instance ',name args))
         (defun ,p-name (object)
           ,(format nil "Return true if OBJECT is of type ~A." name)
           (when (typep object ',name)
             t))
         ,@(mapcar (lambda (name) `(export ',name))
                   exports)
         (export ',make-name)
         (export ',name)
         (export ',p-name)))))
