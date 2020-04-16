;;;; symbols.lisp - utilities for woking with symbols, macros, and definitions

(uiop:define-package #:marie/symbols
  (:use #:cl)
  (:export #:defun*
           #:defvar*
           #:defparameter*
           #:define-constant
           #:define-constant*
           #:define-alias
           #:symbols
           #:with-gensyms
           #:macroexpand*
           #:symbol*))

(in-package #:marie/symbols)

(defmacro defun* (spec args &rest body)
  "Define a function with aliases and optionally export the names. SPEC is
either a single symbol, or a list where the first element is the name of the
function and the rest are aliases. If the last element of SPEC is T, the
function names, including the aliases, are exported."
  (let ((id (if (consp spec) spec (list spec))))
    (destructuring-bind (name &rest handles)
        id
      (let* ((exportp (equal (car (reverse handles)) t))
             (aliases (if exportp (butlast handles) handles)))
        `(progn
           (defun ,name ,args ,@body)
           ,(when exportp `(export ',name))
           ,@(loop :for alias :in aliases
                   :when alias
                   :collect `(progn (define-alias ,name ,alias)
                                    ,(when exportp `(export ',alias)))))))))

(defmacro defvar* (spec &rest body)
  "Define a special variable by DEFVAR and optionally export the name. SPEC is either a
single symbol, or a list where the second element of the list is a boolean
indicating whether to export the symbol or not."
  (let ((id (if (consp spec) spec (list spec))))
    (destructuring-bind (name &optional exportp)
        id
      `(progn
         (defvar ,name ,@body)
         ,(when exportp `(export ',name))))))

(defmacro defparameter* (spec &rest body)
  "Define a special variable by DEFPARAMETER and optionally export the name. SPEC is either a
single symbol, or a list where the second element of the list is a boolean
indicating whether to export the symbol or not."
  (let ((id (if (consp spec) spec (list spec))))
    (destructuring-bind (name &optional exportp)
        id
      `(progn
         (defparameter ,name ,@body)
         ,(when exportp `(export ',name))))))

(defmacro define-constant (name value &optional doc)
  "Create a constant only if it hasn’t been bound or created, yet. SBCL complains
about constants being redefined, hence, this macro."
  (if (boundp name)
      (format t "~&already defined ~A~%old value ~s~%attempted value ~s~%"
              name (symbol-value name) value))
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

(defun call-continue-restart (condition)
  "Call the continue restart on CONDITION."
  (let ((restart (find-restart 'continue condition)))
    (when restart
      (invoke-restart restart))))

(defmacro define-constant* (spec &rest body)
  "Bind NAME to VALUE and only change the binding after subsequent calls to the
macro."
  (let ((id (if (consp spec) spec (list spec))))
    (destructuring-bind (name &optional exportp)
        id
      `(handler-bind #+sbcl ((sb-ext:defconstant-uneql #'call-continue-restart))
                     #-sbcl ((simple-error #'call-continue-restart))
         ,(when exportp `(export ',name))
         (defconstant ,name ,@body)))))

(defmacro define-alias (name alias)
  "Define ALIAS as an alternate name for function NAME."
  `(defun ,alias (&rest args)
     (apply #',name args)))

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
            (format t "~&~A:~%~A" text value-1))
           (t (format t "~&~A-1:~%~A" text value-1)
              (format t "~&~A:~%~A" text value-2)))
     (values)))

(defun symbol* (value)
  "Convert VALUE to a symbol."
  (etypecase value
    (number value)
    (string (intern (string-upcase value)))
    (t value)))
