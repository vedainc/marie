;;;; symbols.lisp - utilities for woking with symbols, macros, and definitions

(uiop:define-package #:marie/symbols
  (:use #:cl)
  (:export #:define-constant
           #:define-constant*
           #:define-alias
           #:defun*
           #:symbols
           #:with-gensyms
           #:macroexpand*
           #:symbol-convert))

(in-package #:marie/symbols)

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

(defmacro define-constant* (name value &optional doc)
  "Bind NAME to VALUE and only change the binding after subsequent calls to the macro."
  `(handler-bind #+sbcl ((sb-ext:defconstant-uneql #'call-continue-restart))
                 #-sbcl ((simple-error #'call-continue-restart))
     (defconstant ,name ,value
       ,@(when doc (list doc)))))

(defmacro define-alias (alias name)
  "Define ALIAS as an alternate name for NAME."
  `(defun ,alias (&rest args)
     (apply #',name args)))

(defmacro defun* (name alias args &rest body)
  "Define a function with an alias."
  `(progn
     (defun ,name ,args ,@body)
     (define-alias ,alias ,name)))

(defmacro symbols (package &key (location :external-symbols))
  "Collect symbols in a package. Prints external symbols by default."
  (let ((pkg (find-package package)))
    `(loop :for symbol :being :the ,location :in ,pkg
           :collect symbol)))

;;; From Practical Common Lisp (2005)—Peter Seibel
(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop :for name :in names :collect `(,name (gensym)))
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

(defun symbol-convert (value)
  "Convert VALUE to a symbol."
  (etypecase value
    (number value)
    (string (intern (string-upcase value)))
    (t value)))
