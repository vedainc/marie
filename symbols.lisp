;;;; symbols.lisp

;;; Utilities for woking with symbols, macros, and definitions

(in-package :mof)

(defmacro defalias (alias name)
  "Create alias `alias' for function `name'."
  `(defun ,alias (&rest args)
     (apply #',name args)))

;;; From Practical Common Lisp—Peter Seibel
(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop :for n :in names :collect `(,n (gensym)))
     ,@body))

(defmacro print-symbols (package &key (location :external-symbols))
  "Print symbols in a package. Prints external symbols by default."
  (let ((pkg (find-package package)))
    `(loop
        :for symbol :being :the ,location :in ,pkg
        :do (format t "~S~%" symbol))))

;;; From A Gentle Introduction to Symbolic Computation—David Touretzky
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
