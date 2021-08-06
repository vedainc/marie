;;;; defs.lisp

(uiop:define-package #:marie/defs
  (:use #:cl)
  (:export #:def
           #:defm
           #:defv
           #:defp
           #:defc))

(in-package #:marie/defs)

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
       (export ',name)
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
       (export ',name)
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
       (export ',name)
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
       (export ',name)
       ,@(loop :for alias :in aliases
               :when alias
               :collect `(progn (defparameter ,alias ,@body)
                                (export ',alias))))))

(defmacro deft (spec &rest body)
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

(defmacro defc (name (&rest superclasses) (&rest slot-specs)
                &optional class-option)
  "Define a class like DEFCLASS. If an element in SLOT-SPECS contains `:export t`, export that slot. If CLASS-OPTION contains `:export t`, export the entire class."
  (let ((exports (mapcan (lambda (spec)
                           (when (getf (cdr spec) :export)
                             (let ((name (or (getf (cdr spec) :accessor)
                                             (getf (cdr spec) :reader)
                                             (getf (cdr spec) :writer))))
                               (when name (list name)))))
                         slot-specs)))
    `(progn
       (defclass ,name (,@superclasses)
         ,(append
           (mapcar (lambda (spec)
                     (let ((export-pos (position :export spec)))
                       (if export-pos
                           (append (subseq spec 0 export-pos)
                                   (subseq spec (+ 2 export-pos)))
                           spec)))
                   slot-specs)
           (when class-option (list class-option))))
       ,@(mapcar (lambda (name) `(export ',name))
                 exports)
       ,(when (getf class-option :export)
          `(export ',name)))))
