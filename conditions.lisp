;;;; conditions.lisp

(uiop:define-package #:marie/conditions
  (:use #:cl
        #:marie/defs))

(in-package #:marie/conditions)

(defm map-and (fn &rest args)
  "Return true if FN returns true for all items in ARGS."
  `(and ,@(loop :for arg :in args :collect `(funcall ,fn ,arg))
        t))

(defm map-or (fn &rest args)
  "Return true if FN returns true for at least one item in ARGS."
  `(or ,@(loop :for arg :in args :collect `(funcall ,fn ,arg))
       nil))

(defm rmap-and (value &rest fns)
  "Return true if all functions in FNS return true for VALUE."
  `(and ,@(loop :for fn :in fns :collect `(funcall ,fn ,value))
        t))

(defm rmap-or (value &rest fns)
  "Return true if at least one function in FNS return true for VALUE."
  `(or ,@(loop :for fn :in fns :collect `(funcall ,fn ,value))
       nil))

(defm (logical-and ∧) (&body body)
  "Return true if all forms in BODY evaluates to true."
  `(when (and ,@body)
     t))

(defm (logical-or ∨) (&body body)
  "Return true if all forms in BODY evaluates to false."
  `(when (or ,@body)
     t))

(defm ¬ (arg)
  "Return the negation of ARG."
  `(not ,arg))

(defm when-let (bindings &body forms)
  "Use BINDINGS like with LET, then evaluate FORMS if all BINDINGS evaluate to a
true value. This is ALEXANDRIA:WHEN-LET."
  (let* ((binding-list (if (and (consp bindings) (symbolp (car bindings)))
                           (list bindings)
                           bindings))
         (variables (mapcar #'car binding-list)))
    `(let ,binding-list
       (when (and ,@variables)
         ,@forms))))

(defm when-let* (bindings &body body)
  "Use BINDINGS like with LET*, then evaluate FORMS if all BINDINGS evaluate to
a true value. This is ALEXANDRIA:WHEN-LET*."
  (let ((binding-list (if (and (consp bindings) (symbolp (car bindings)))
                          (list bindings)
                          bindings)))
    (labels ((bind (bindings body)
               (if bindings
                   `(let (,(car bindings))
                      (when ,(caar bindings)
                        ,(bind (cdr bindings) body)))
                   `(progn ,@body))))
      (bind binding-list body))))

(def true-false-p (x y)
  "Return true if X is true and Y is false."
  (if (and x (null y)) t nil))

(def false-true-p (x y)
  "Return true if X is false and Y is true."
  (true-false-p y x))

(def true-true-p (x y)
  "Return true if X is true and Y is true."
  (and x y t))

(defmacro aif (test-form then-form &optional else-form)
  "Anaphoric IF."
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defm awhen (condition &body body)
  "Anaphoric WHEN."
  `(let ((it ,condition))
     (when it
       ,@body)))

