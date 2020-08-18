;;;; symbols.lisp

(uiop:define-package #:marie/symbols
  (:use #:cl)
  (:export #:define-alias
           #:defun*
           #:defmacro*
           #:defvar*
           #:defparameter*
           #:defconstant*
           #:define-constant
           #:symbols
           #:with-gensyms
           #:macroexpand*
           #:symbol*
           #:mapply
           #:forget
           #:rename))

(in-package #:marie/symbols)

(defmacro define-alias (name alias)
  "Define ALIAS as an alternate name for function NAME."
  `(defun ,alias (&rest args)
     (apply #',name args)))

(defmacro defun* (spec args &rest body)
  "Define a function with aliases and export the names. SPEC is either a single symbol, or a list
where the first element is the name of the function and the rest are aliases."
  (let ((id (if (consp spec) spec (list spec))))
    (destructuring-bind (name &rest aliases)
        id
      `(progn
         (defun ,name ,args ,@body)
         (export ',name)
         ,@(loop :for alias :in aliases
                 :when alias
                 :collect `(progn (setf (fdefinition ',alias) (fdefinition ',name))
                                  (export ',alias)))))))

(defmacro defmacro* (spec &rest body)
  "Define a macro with aliases and export the names. SPEC is either a single symbol, or a list where
the first element is the name of the function and the rest are aliases."
  (let ((id (if (consp spec) spec (list spec))))
    (destructuring-bind (name &rest aliases)
        id
      `(progn
         (defmacro ,name ,@body)
         (export ',name)
         ,@(loop :for alias :in aliases
                 :when alias
                 :collect `(progn (setf (macro-function ',alias) (macro-function ',name))
                                  (export ',alias)))))))

(defmacro defvar* (spec &rest body)
  "Define a special variable by DEFVAR with aliases and export the names. SPEC is either a single
symbol, or a list where the first element is the name of the function and the rest are aliases."
  (let ((id (if (consp spec) spec (list spec))))
    (destructuring-bind (name &rest aliases)
        id
      `(progn
         (defvar ,name ,@body)
         (export ',name)
         ,@(loop :for alias :in aliases
                 :when alias
                 :collect `(progn (defvar ,alias ,@body)
                                  (export ',alias)))))))

(defmacro defparameter* (spec &rest body)
  "Define a special variable by DEFPARAMETER and export the names. SPEC is either a single symbol,
or a list where the first element is the name of the function and the rest are aliases."
  (let ((id (if (consp spec) spec (list spec))))
    (destructuring-bind (name &rest aliases)
        id
      `(progn
         (defparameter ,name ,@body)
         (export ',name)
         ,@(loop :for alias :in aliases
                 :when alias
                 :collect `(progn (defparameter ,alias ,@body)
                                  (export ',alias)))))))

(defun call-continue-restart (condition)
  "Call the continue restart on CONDITION."
  (let ((restart (find-restart 'continue condition)))
    (when restart
      (invoke-restart restart))))

(defmacro defconstant* (spec &rest body)
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

(defmacro define-constant (name value &optional doc)
  "Create a constant only if it hasn’t been bound or created, yet. SBCL complains about constants
being redefined, hence, this macro."
  (if (boundp name)
      (format t "~&already defined ~A~%old value ~s~%attempted value ~s~%"
              name (symbol-value name) value))
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

(defmacro symbols (package &key (location :external-symbols))
  "Collect symbols in a package. Prints external symbols by default."
  (let ((pkg (find-package package)))
    `(loop :for symbol :being :the ,location :in ,pkg
           :collect symbol)))

;;; From Practical Common Lisp (2005)—Peter Seibel
(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop :for napme :in names :collect `(,name (gensym)))
     ,@body))

(defmacro macroexpand* (form)
  "Pretty print the macro expansion of FORM."
  `(let* ((text "MACROEXPAND")
          (value-1 (macroexpand-1 ,form))
          (value-2 (macroexpand ,form)))
     (cond ((equal value-1 value-2)
            (format t "~&~S:~%~S" text value-1))
           (t (format t "~&~S-1:~%~S" text value-1)
              (format t "~&~S:~%~S" text value-2)))
     (values)))

(defun symbol* (value)
  "Convert VALUE to a symbol."
  (etypecase value
    (number value)
    (string (intern (string-upcase value)))
    (t value)))

(defmacro mapply (macro &rest args)
  "Apply macro MACRO to each item in ARGS."
  `(progn
     ,@(loop :for arg :in args :collect `(,macro ,arg))))

(defmacro forget (arg-1 &optional arg-2)
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
  "Rename special variable NAME-1 to NAME-2."
  `(if (boundp ',name-1)
       (let ((value ,name-1))
         (defparameter ,name-2 value)
         (forget ,name-1))
       (values)))
