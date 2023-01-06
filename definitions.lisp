;;;; -*- mode: lisp; syntax: common-lisp; base: 10; coding: utf-8-unix; external-format: (:utf-8 :eol-style :lf); -*-
;;;; definitions.lisp: exporting replacements for functions, macros, and other things

(uiop:define-package #:marie/definitions
  (:use #:cl)
  (:export #:defm #:defm-))

(in-package #:marie/definitions)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *docstring*
    ", and conditionally export NAMES.

NAMES is either a single symbol, or a list of symbols where the first element is
the name of the function and the rest are aliases. export NAMES if T is present
in NAMES."
    "The common docstring in the definers.")

  (defvar *name-separator*
    '(#\◆)
    "The list of characters used to separate names and aliases in definitions.")

  (defun compose-docstring (text)
    "Return a docstring suitable for a definer."
    (format nil "~A~A" text *docstring*))

  (defun string-empty-p (string)
    "Return true if STRING is empty."
    (= (length string) 0))

  (defun split-names (names)
    "Split NAMES by delimiters."
    (let* ((string (string names))
           (split (uiop:split-string string :separator *name-separator*))
           (strings (remove-if #'string-empty-p split)))
      (mapcar #'read-from-string strings)))

  (defun tack-t (names)
    "Concatename `◆t' to names."
    (let ((separator (string (first *name-separator*)))
          (string (string names)))
      (read-from-string (uiop:strcat string separator "t")))))

(defmacro export-names (name aliases)
  "Return a stub for exporting names in definers."
  `(when (member t ',aliases)
     (progn
       (export ',name)
       ,@(loop :for alias :in (remove t aliases)
               :collect `(export ',alias)))))

(defmacro %defm (names args &rest body)
  #.(compose-docstring "Define macros")
  (destructuring-bind (name &rest aliases)
      (split-names names)
    `(progn
       (defmacro ,name ,args ,@body)
       ,@(loop :for alias :in (remove t aliases)
               :collect `(setf (macro-function ',alias) (macro-function ',name)))
       (export-names ,name ,aliases))))

(defmacro defm (names args &rest body)
  "Define macros with DEFMACRO.

The forms

    (defm qux (op) `(progn (,op 1)))
    (defm (quux corge) (op) `(progn (,op 2)))

define the macros QUX, QUUX, and CORGE; and export those names."
  `(%defm ,(tack-t names) ,args ,@body))

(defmacro defm- (names args &rest body)
  "Like DEFM, but do not export NAMES."
  `(%defm ,names ,args ,@body))

(defm- %def (names args &rest body)
  #.(compose-docstring "Define functions")
  (destructuring-bind (name &rest aliases)
      (split-names names)
    `(progn
       (defun ,name ,args ,@body)
       ,@(loop :for alias :in (remove t aliases)
               :collect `(setf (fdefinition ',alias) (fdefinition ',name)))
       (export-names ,name ,aliases))))

(defm def (names args &rest body)
  "Define functions with DEFUN.

The forms

    (def foo (n) (1- n))
    (def bar◆baz (n) (+ n 1))

define the functions FOO, BAR, and BAZ; and export those names."
  `(%def ,(tack-t names) ,args ,@body))

(defm def- (names args &rest body)
  "Like DEF, but do not export NAMES."
  `(%def ,names ,args ,@body))

(defm- %defv (names &rest body)
  #.(compose-docstring "Define special variables with DEFVAR")
  (destructuring-bind (name &rest aliases)
      (split-names names)
    `(progn
       (defvar ,name ,@body)
       ,@(loop :for alias :in (remove t aliases)
               :collect `(defvar ,alias ,@body))
       (export-names ,name ,aliases))))

(defm defv (names &rest body)
  "Define special variables with DEFVAR.

The forms

    (defv *grault* nil \"The grault.\")
    (defv *garply*◆*waldo* nil \"Like grault.\")

define the special variables *GRAULTY*, *GARPY*, and *WALDO*; and export those
names."
  `(%defv ,(tack-t names) ,@body))

(defm defv- (names &rest body)
  "Like DEFV, but do not export NAMES."
  `(%defv ,names ,@body))

(defm- %defp (names &rest body)
  #.(compose-docstring "Define special variables with DEFPARAMETER")
  (destructuring-bind (name &rest aliases)
      (split-names names)
    `(progn
       (defparameter ,name ,@body)
       ,@(loop :for alias :in (remove t aliases)
               :collect `(defparameter ,alias ,@body))
       (export-names ,name ,aliases))))

(defm defp (names &rest body)
  "Define special variables with DEFPARAMETER.

The forms

    (defp *grault* nil \"The grault.\")
    (defp *garply*◆*waldo* nil \"Like grault.\")

define the special variables *GRAULTY*, *GARPY*, and *WALDO*; and export those
names."
  `(%defp ,(tack-t names) ,@body))

(defm defp- (names &rest body)
  "Like DEFP, but do not export NAMES."
  `(%defp ,names ,@body))

(defm- %defk (names &rest body)
  #.(compose-docstring "Define constants with DEFCONSTANT but allow the definitions to change on subsequent calls")
  (destructuring-bind (name &rest aliases)
      (split-names names)
    `(handler-bind
         #+sbcl ((sb-ext:defconstant-uneql #'continue))
       #-sbcl ((simple-error #'continue))
       (defconstant ,name ,@body)
       ,@(loop :for alias :in (remove t aliases)
               :collect `(defconstant ,alias ,@body))
       (export-names ,name ,aliases))))

(defm defk (names &rest body)
  "Define constants with %DEFK.

The forms

    (defk +fred+ nil \"The Fred constant.\")
    (defk +plugh+◆+xyzzy+ nil \"Like Fred.\")

define the constants +FRED+, +PLUGH+, +XYZZY+; and export those names."
  `(%defk ,(tack-t names) ,@body))

(defm defk- (names &rest body)
  "Like DEFK, but do not export NAMES."
  `(%defk ,names ,@body))

(defm- %defg (names (&rest parameters) &body body)
  #.(compose-docstring "Define generic functions")
  (destructuring-bind (name &rest aliases)
      (split-names names)
    `(progn
       (defgeneric ,name (,@parameters) ,@body)
       ,@(loop :for alias :in (remove t aliases)
               :collect `(defgeneric ,alias (,@parameters) ,@body))
       (export-names ,name ,aliases))))

(defm defg (names (&rest parameters) &rest body)
  "Define generic functions with DEFGENERIC

The forms

    (defg delete (volume registry)
      (:documentation \"Delete VOLUME in REGISTRY.\"))

    (defg create◆update (volume registry)
      (:documentation \"Update VOLUME in REGISTRY.\"))

define the generic functions DELETE, CREATE, and UPDATE; and export those names."
  `(%defg ,(tack-t names) ,parameters ,@body))

(defm defg- (names (&rest parameters) &rest body)
  "Like DEFG, but do not export NAMES."
  `(%defg ,names ,parameters ,@body))

(defm- %deft (names &body body)
  #.(compose-docstring "Define methods with DEFMETHOD")
  (destructuring-bind (name &rest aliases)
      (split-names names)
    (let ((aliases-1 (remove t aliases)))
      (if (keywordp (first body))
          (destructuring-bind (type (&rest parameters) &body content)
              body
            `(progn
               (defmethod ,name ,type (,@parameters) ,@content)
               ,@(loop :for alias :in aliases-1
                       :collect `(defmethod ,alias ,type (,@parameters) ,@content))
               (export-names ,name ,aliases)))
          (destructuring-bind ((&rest parameters) &body content)
              body
            `(progn
               (defmethod ,name (,@parameters) ,@content)
               ,@(loop :for alias :in aliases-1
                       :collect `(defmethod ,alias (,@parameters) ,@content))
               (export-names ,name ,aliases)))))))

(defm deft (names &rest body)
  "Define methods with DEFMETHOD.

The forms

    (deft current ((o null))
      \"Return T on null objects\"
      t)

    (deft prev◆next ((o null))
      \"Return NIL on null objects.\"
      nil)

define the methods CURRENT, PREV, and NEXT; and export those names."
  `(%deft ,(tack-t names) ,@body))

(defm deft- (names &rest body)
  "Like DEFT, but do not export NAMES."
  `(%deft ,names ,@body))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun p-symbol (symbol)
    "Return a conditionally hyphenated predicate symbol."
    (let* ((string (prin1-to-string symbol))
           (split (uiop:split-string string :separator '(#\-))))
      (if (> (length split) 1)
          (read-from-string (format nil "~{~A~^-~}" (append split '("P"))))
          (read-from-string (format nil "~{~A~^-~}P" split)))))

  (defun compose-name (predicate &rest names)
    "Compose a hyphenated symbol from NAMES. Return a symbol for predicate use
if PREDICATE is true."
    (let ((val (read-from-string
                (format nil "~{~A~^-~}"
                        (mapcar (lambda (name)
                                  (string-upcase (string name)))
                                names)))))
      (if predicate
          (p-symbol val)
          val))))

(defm- compose-definitions (name superclasses slot-specs &optional class-option)
  "Compose the forms for creating a class."
  `(progn
     (defclass ,name (,@superclasses)
       ,@(append (list slot-specs)
          (when class-option
            (list class-option))))
     (defun ,(compose-name nil 'make name) (&rest args)
       ,(format nil "Return a new instance of ~A." name)
       (apply #'make-instance ',name args))
     (defun ,(compose-name t name) (object)
       ,(format nil "Return true if OBJECT is of type ~A." name)
       (when (typep object ',name)
         t))))

(defm- compose-exports (name)
  "Compose the forms for exporting symbols."
  `(progn
     (export ',(compose-name nil 'make name))
     (export ',name)
     (export ',(compose-name t name))))

(defm- %defc (names (&rest superclasses)
                 (&rest slot-specs) &optional class-option)
  "Define classes with DEFCLASS."
  (destructuring-bind (name &rest aliases)
      (split-names names)
    (let ((exports (mapcan (lambda (spec)
                             (let ((name (or (getf (cdr spec) :accessor)
                                             (getf (cdr spec) :reader)
                                             (getf (cdr spec) :writer))))
                               (when name (list name))))
                           slot-specs)))
      `(progn
         (compose-definitions ,name ,superclasses
                              ,slot-specs ,class-option)
         ,@(loop :for alias :in (remove t aliases)
                 :collect `(compose-definitions ,alias ,superclasses
                                                ,slot-specs ,class-option))
         (when (member t ',aliases)
           (progn
             ,@(mapcar (lambda (name) `(export ',name))
                       exports)
             (compose-exports ,name)
             ,@(loop :for alias :in (remove t aliases)
                     :collect `(compose-exports ,alias))))))))

(defm defc (names (&rest superclasses) (&rest slot-specs) &optional class-option)
  "Define classes with DEFCLASS.

The form

    (defc unitxblank (frame)
      ((id :initarg :id
           :initform -1
           :reader id
           :documentation \"The unique numeric ID of a hole in a registry.\")
       (name :initarg :name
             :initform \"\"
             :reader name
             :documentation \"The name of the hole.\"))
      (:documentation \"An empty frame.\"))

defines the classes UNIT and BLANK whose superclass is FRAME. In addition to
that, it creates MAKE-UNIT◆instantiator for UNIT much like with DEFSTRUCT; and
UNITP◆predicate to test if an object is an instance of UNIT. The same is also
created for BLANK. Those symbols are exported along with the names of the
classes.
"
  `(%defc ,(tack-t names) ,superclasses ,slot-specs ,class-option))

(defm defc- (names (&rest superclasses) (&rest slot-specs) &optional class-option)
  "Like DEFC, but do not export NAMES."
  `(%defc ,names ,superclasses ,slot-specs ,class-option))

(defmacro %defmm (names args function &optional doc)
  #.(compose-docstring "Define modify macros")
  (destructuring-bind (name &rest aliases)
      (split-names names)
    `(progn
       (define-modify-macro ,name ,args ,function ,doc)
       ,@(loop :for alias :in (remove t aliases)
               :collect `(define-modify-macro ,alias ,args ,function ,doc))
       (export-names ,name ,aliases))))

(defm defmm (names args &rest body)
  "Define modify macros with DEFINE-MODIFY-MACRO.

The forms

    ...

define the modify macros ...; and export those names."
  `(%defmm ,(tack-t names) ,args ,@body))

(defm defmm- (names args &rest body)
  "Like DEFMM, but do not export NAMES."
  `(%defmm ,names ,args ,@body))

(defm with-binding◆let1 ((name value) &rest body)
  "Like LET but for single values only."
  `(let ((,name ,value))
     ,@body))
