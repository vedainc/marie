;;;; defs.lisp
;;;; Exporting replacements for functions and macros that create definitions and bindings

(uiop:define-package #:marie/definitions
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
           #:defc-
           #:defc))

(in-package #:marie/definitions)

(defmacro export-names (name aliases)
  "Return a stub for exporting names in definers."
  `(when (member t ',aliases)
     (progn
       (export ',name)
       ,@(loop :for alias :in (remove t aliases)
               :collect `(export ',alias))))  )

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *docstring*
    ", and conditionally export the names.

NAMES is either a single symbol, or a list of symbols where the first element is the name of the function and the rest are aliases. Export the symbols if T is present in NAMES."
    "The common docstring in the definers.")

  (defun compose-docstring (text)
    "Return a docstring suitable for a definer."
    (format nil "~A~A" text *docstring*)))

(defmacro %def (names args &rest body)
  #.(compose-docstring "Define functions")
  (destructuring-bind (name &rest aliases)
      (uiop:ensure-list names)
    `(progn
       (defun ,name ,args ,@body)
       ,@(loop :for alias :in (remove t aliases)
               :collect `(setf (fdefinition ',alias) (fdefinition ',name)))
       (export-names ,name ,aliases))))

(defmacro def (names args &rest body)
  "Define functions with DEFUN.

The forms

    (def foo (n) (1- n))
    (def (bar baz) (n) (+ n 1))

define the functions FOO, BAR, and BAZ; and export those names."
  `(%def ,(append (uiop:ensure-list names) (list t)) ,args ,@body))

(defmacro def- (names args &rest body)
  "Like DEF, but do not export the names."
  `(%def ,names ,args ,@body))

(defmacro %defm (names args &rest body)
  #.(compose-docstring "Define macros")
  (destructuring-bind (name &rest aliases)
      (uiop:ensure-list names)
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
  `(%defm ,(append (uiop:ensure-list names) (list t)) ,args ,@body))

(defmacro defm- (names args &rest body)
  "Like DEFM, but do not export the names."
  `(%defm ,names ,args ,@body))

(defmacro %defv (names &rest body)
  #.(compose-docstring "Define special variables with DEFVAR")
  (destructuring-bind (name &rest aliases)
      (uiop:ensure-list names)
    `(progn
       (defvar ,name ,@body)
       ,@(loop :for alias :in (remove t aliases)
               :collect `(defvar ,alias ,@body))
       (export-names ,name ,aliases))))

(defmacro defv (names &rest body)
  "Define special variables with DEFVAR.

The forms

    (defv *grault* nil \"The grault.\")
    (defv (*garply* *waldo*) nil \"Like grault.\")

define the special variables *GRAULTY*, *GARPY*, and *WALDO*; and export those names."
  `(%defv ,(append (uiop:ensure-list names) (list t)) ,@body))

(defmacro defv- (names &rest body)
  "Like DEFV, but do not export the names."
  `(%defv ,names ,@body))

(defmacro %defp (names &rest body)
  #.(compose-docstring "Define special variables with DEFPARAMETER")
  (destructuring-bind (name &rest aliases)
      (uiop:ensure-list names)
    `(progn
       (defparameter ,name ,@body)
       ,@(loop :for alias :in (remove t aliases)
               :collect `(defparameter ,alias ,@body))
       (export-names ,name ,aliases))))

(defmacro defp (names &rest body)
  "Define special variables with DEFPARAMETER.

The forms

    (defp *grault* nil \"The grault.\")
    (defp (*garply* *waldo*) nil \"Like grault.\")

define the special variables *GRAULTY*, *GARPY*, and *WALDO*; and export those names."
  `(%defp ,(append (uiop:ensure-list names) (list t)) ,@body))

(defmacro defp- (names &rest body)
  "Like DEFP, but do not export the names."
  `(%defp ,names ,@body))

(defmacro %defk (names &rest body)
  #.(compose-docstring "Define constants with DEFCONSTANT but allow the definitions to change on subsequent calls")
  (let ((id (if (consp names) names (list names))))
    (destructuring-bind (name &rest aliases)
        id
      `(handler-bind
           #+sbcl ((sb-ext:defconstant-uneql #'continue))
         #-sbcl ((simple-error #'continue))
         (defconstant ,name ,@body)
         ,@(loop :for alias :in (remove t aliases)
                 :collect `(defconstant ,alias ,@body))
         (export-names ,name ,aliases)))))

(defmacro defk (names &rest body)
  "Define constants with %DEFK.

The forms

    (defk +fred+ nil \"The Fred constant.\")
    (defk (+plugh+ +xyzzy+) nil \"Like Fred.\")

define the constants +FRED+, +PLUGH+, +XYZZY+; and export those names."
  `(%defk ,(append (uiop:ensure-list names) (list t)) ,@body))

(defmacro defk- (names &rest body)
  "Like DEFK, but do not export the names."
  `(%defk ,names ,@body))

(defmacro %defg (names (&rest parameters) &body body)
  #.(compose-docstring "Define generic functions")
  (destructuring-bind (name &rest aliases)
      (uiop:ensure-list names)
    `(progn
       (defgeneric ,name (,@parameters) ,@body)
       ,@(loop :for alias :in (remove t aliases)
               :collect `(defgeneric ,alias (,@parameters) ,@body))
       (export-names ,name ,aliases))))

(defmacro defg (names (&rest parameters) &rest body)
  "Define generic functions with DEFGENERIC

The forms

    (defg delete (volume registry)
      (:documentation \"Delete VOLUME in REGISTRY.\"))

    (defg (create update) (volume registry)
      (:documentation \"Update VOLUME in REGISTRY.\"))

define the generic functions DELETE, CREATE, and UPDATE; and export those names."
  `(%defg ,(append (uiop:ensure-list names) (list t)) ,parameters ,@body))

(defmacro defg- (names (&rest parameters) &rest body)
  "Like DEFG, but do not export the names."
  `(%defg ,names ,parameters ,@body))

(defmacro %deft (names &body body)
  #.(compose-docstring "Define methods with DEFMETHOD")
  (destructuring-bind (name &rest aliases)
      (uiop:ensure-list names)
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

(defmacro deft (names &rest body)
  "Define methods with DEFMETHOD.

The forms

    (deft current ((o null))
      \"Return T on null objects\"
      t)

    (deft (prev next) ((o null))
      \"Return NIL on null objects.\"
      nil)

define the methods CURRENT, PREV, and NEXT; and export those names."
  `(%deft ,(append (uiop:ensure-list names) (list t)) ,@body))

(defmacro deft- (names &rest body)
  "Like DEFT, but do not export the names."
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
    "Compose a hyphenated symbol from NAMES. Return a symbol for predicate use if PREDICATE is true."
    (let ((val (read-from-string
                (format nil "~{~A~^-~}"
                        (mapcar (lambda (name)
                                  (string-upcase (string name)))
                                names)))))
      (if predicate
          (p-symbol val)
          val))))

(defmacro compose-definitions (name superclasses slot-specs &optional class-option)
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

(defmacro compose-exports (name)
  "Compose the forms for exporting symbols."
  `(progn
     (export ',(compose-name nil 'make name))
     (export ',name)
     (export ',(compose-name t name))))

(defmacro %defc (names (&rest superclasses)
                 (&rest slot-specs) &optional class-option)
  "Define classes with DEFCLASS."
  (destructuring-bind (name &rest aliases)
      (uiop:ensure-list names)
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

(defmacro defc- (names (&rest superclasses) (&rest slot-specs) &optional class-option)
  "Define classes with DEFCLASS.

The expression

    (defc- (unit blank) (frame)
      ((id :initarg :id
           :initform -1
           :reader id
           :documentation \"The unique numeric ID of a hole in a registry.\")
       (name :initarg :name
             :initform \"\"
             :reader name
             :documentation \"The name of the hole.\"))
      (:documentation \"An empty frame.\"))

defines the classes UNIT and BLANK whose superclass is FRAME. In addition to that, it creates MAKE-UNIT—instantiator for UNIT much like with DEFSTRUCT; and UNITP—predicate to test if an object is an instance of UNIT. The same is also created for BLANK. Those symbols are not exported.
"
  `(%defc ,names ,superclasses ,slot-specs ,class-option))

(defmacro defc (names (&rest superclasses) (&rest slot-specs) &optional class-option)
  "Define classes with DEFCLASS.

The expression

    (defc (unit blank) (frame)
      ((id :initarg :id
           :initform -1
           :reader id
           :documentation \"The unique numeric ID of a hole in a registry.\")
       (name :initarg :name
             :initform \"\"
             :reader name
             :documentation \"The name of the hole.\"))
      (:documentation \"An empty frame.\"))

defines the classes UNIT and BLANK whose superclass is FRAME. In addition to that, it creates MAKE-UNIT—instantiator for UNIT much like with DEFSTRUCT; and UNITP—predicate to test if an object is an instance of UNIT. The same is also created for BLANK. Those symbols are exported along with the names of the classes.
"
  `(%defc ,(append (uiop:ensure-list names) (list t)) ,superclasses,slot-specs ,class-option))
