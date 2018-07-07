;;;; symbols.lisp

;;; Utilities for woking with symbols, macros, and definitions

(in-package #:mof)

(defmacro defcon (name value &optional doc)
  "Create a constant only if it hasn’t been bound or created, yet. SBCL complains
about constants being redefined, hence, this macro."
  (if (boundp name)
      (format t
              "~&already defined ~A~%old value ~s~%attempted value ~s~%"
              name (symbol-value name) value))
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))


(defmacro defalias (alias name)
  "Create alias `alias' for function `name'."
  `(defun ,alias (&rest args)
     (apply #',name args)))

(defmacro defun* (name alias args &rest body)
  "Define a function with an alias."
  `(progn
     (defun ,name ,args ,@body)
     (defalias ,name ,alias)))

(defmacro symbols (package &key (location :external-symbols))
  "Collect symbols in a package. Prints external symbols by default."
  (let ((pkg (find-package package)))
    `(loop :for symbol :being :the ,location :in ,pkg
           :collect symbol)))

;;; From Practical Common Lisp - Peter Seibel
(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop :for n :in names :collect `(,n (gensym)))
     ,@body))

;;; From A Gentle Introduction to Symbolic Computation — David Touretzky
(defmacro ppmx (form)
  "Pretty prints the macro expansion of FORM."
  `(let* ((exp1 (macroexpand-1 ',form))
          (exp (macroexpand exp1))
          (*print-circle* nil))
     (cond ((equal exp exp1)
            (format t "~&Macro expansion:")
            (pprint exp))
           (t (format t "~&First step of expansion:")
              (pprint exp1)
              (format t "~%~%Final expansion:")
              (pprint exp)))
     (format t "~%~%")
     (values)))
