;;;; -*- mode: lisp; syntax: common-lisp; base: 10; coding: utf-8-unix; external-format: (:utf-8 :eol-style :lf); -*-
;;;; conditionals.lisp --- utilities for handling conditional expressions

(uiop:define-package #:marie/src/conditionals
  (:use #:cl
        #:marie/src/definitions
        #:marie/src/symbols)
  (:export #:it))

(in-package #:marie/src/conditionals)


;;; Mapping fns

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


;;; Logical operator variants

(defm logical-and^land^∧ (&body body)
  "Return true if all forms in BODY evaluates to true."
  `(ω (and ,@body)))

(defm logical-or^lor^∨ (&body body)
  "Return true if at least one form in BODY evaluates to true."
  `(ω (or ,@body)))

(defm negation^neg^¬ (arg)
  "Return the negation of ARG."
  (let ((value (gensym)))
    `(let ((,value ,arg))
       (cond ((integerp ,value) (- ,value))
             ((functionp ,value) (complement ,value))
             (t (not ,value))))))

(defm logical-and-not^∧¬ (arg1 arg2)
  "Return true if ARG1 is true and ARG2 is not true."
  `(ω (and ,arg1 (not ,arg2))))

(defm logical-not-and^¬∧ (arg1 arg2)
  "Return true if ARG1 is not true and ARG2 is true."
  `(ω (and (not ,arg1) ,arg2)))

(defm logical-or-not^∨¬ (arg1 arg2)
  "Return true if ARG1 is true or ARG2 is not true."
  `(ω (or ,arg1 (not ,arg2))))

(defm logical-not-or^¬∨ (arg1 arg2)
  "Return true if ARG1 is not true or ARG2 is true."
  `(ω (or (not ,arg1) ,arg2)))

(defm logical-not-not^¬¬ (arg1 arg2)
  "Return true if ARG1 is not true and ARG2 is not true."
  `(ω (and (not ,arg1) (not ,arg2))))


;;; When macro bindings

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


;;; Boolean logic helpers

(defm true-when^ω (condition)
  "Return T when CONDITION evaluates as true."
  `(when ,condition
     t))

(def true-false-p (x y)
  "Return true if X is true and Y is false."
  (declare (type boolean x y))
  (if (and x (null y)) t nil))

(def false-true-p (x y)
  "Return true if X is false and Y is true."
  (declare (type boolean x y))
  (true-false-p y x))

(def true-true-p (x y)
  "Return true if X is true and Y is true."
  (declare (type boolean x y))
  (and x y t))

(defm aif (test-form then-form &optional else-form)
  "Anaphoric IF."
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defm awhen (test-form &body then-form)
  "Anaphoric WHEN."
  `(aif ,test-form
    (progn ,@then-form)))

(defm aand (&rest args)
  "Anaphoric AND."
  (cond ((null args) t)
        ((null (cdr args)) (car args))
        (t `(aif ,(car args) (aand ,@(cdr args))))))

(defm acond (&rest clauses)
  "Anaphoric COND."
  (if (null clauses)
      nil
      (let ((clause (car clauses))
            (symbol (gensym)))
        `(let ((,symbol ,(car clause)))
           (if ,symbol
               (let ((it ,symbol)) ,@(cdr clause))
               (acond ,@(cdr clauses)))))))

(defm nif (test-form then-form &optional else-form)
  "Not IF."
  `(if (not ,test-form)
       ,then-form
       ,else-form))

(def- alpha-reader (stream char)
  "Define the reader for α, so that it can be used to refer to the anaphora."
  (declare (ignore stream char))
  'MARIE/SRC/CONDITIONALS::IT)

(def- use-alpha-reader ()
  "Put the alpha reader into effect."
  (set-macro-character #\α #'alpha-reader))

(use-alpha-reader)
