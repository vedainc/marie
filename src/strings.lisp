;;;; -*- mode: lisp; syntax: common-lisp; base: 10; coding: utf-8-unix; external-format: (:utf-8 :eol-style :lf); -*-
;;;; strings.lisp --- utilities for dealing with strings

(uiop:define-package #:marie/src/strings
  (:use #:cl
        #:marie/src/definitions
        #:marie/src/sequences
        #:marie/src/conditionals))

(in-package #:marie/src/strings)

(def empty-string-p (string)
  "Return true if STRING is of length zero."
  (zerop (length string)))

(def string* (object)
  "Return OBJECT as a string."
  (etypecase object
    (number (format nil "~A" object))
    (cons (format nil "(~{~A~^ ~})" object))
    (string object)
    (t (string object))))

;; NOTE: superseded by UIOP:STRCAT
(def concat^cat (&rest args)
  "Concatenate ARGS to a string."
  (let ((value (loop :for arg :in args :collect (string* arg))))
    (apply #'concatenate 'string value)))

(def reduce-concat^red-cat (&rest args)
  "Reduce ARGS with CONCAT."
  (flet ((fn (arg)
             (reduce #'concat arg)))
    (if (length= args 1)
        (fn (car args))
        (fn args))))

(def intern-concat^int-cat (package &rest args)
  "Concatenate ARGS to a string then intern it to the current package."
  (let ((p (if (null package) *package* package)))
    (intern (apply #'concat args) (find-package p))))

(def string-list (string)
  "Create a list from STRING."
  (loop :for char :across string :collect char))

(def normalize-strings (list &key (character #\_))
  "Return list of characters with equal length using CHARACTER as end padding."
  (assert (>= (length list) 1))
  (let ((max (apply #'max (mapcar #'length list))))
    (loop :for item :in list
          :for length = (length item)
          :if (= length max) :collect item
            :else
              :collect (cat item (make-string (- max length) :initial-element character)))))

(def fmt (&rest args)
  "Simply return a string with FORMAT."
  (apply #'format nil args))

(def fmt* (&rest args)
  "Print ARGS to stdout with FORMAT."
  (apply #'format t args))

(def list-string (list &optional (converter 'string*))
  "Return the string version of LIST."
  (labels ((fn (args &optional acc)
               (cond ((null args) (funcall converter (nreverse acc)))
                     ((consp (car args))
                      (fn (cdr args)
                          (cons (fn (car args) nil)
                                acc)))
                     (t (fn (cdr args) (cons (car args) acc))))))
    (fn list)))

(def genstr (&optional (prefix "G"))
  "Return a random string."
  (string (gensym prefix)))

(def earmuff (&rest args)
  "Return a hyphenated symbol from ARGS with surrounding *s."
  (read-from-string (format nil "*~:@(~{~A~^-~}~)*" args)))

(def separators (string &optional (filter #'alphanumericp))
  "Return the separators used in STRING, applying FILTER to remove characters."
  (loop :for char :across (remove-if filter string)
        :collecting char :into chars
        :finally (return (remove-duplicates chars))))

(def strict-substring-p (x y)
  "Return true if X is part of Y, and that X is found from the start of Y."
  (∧ (not (= (length x)
             (length y)))
     (let ((val (search x y)))
       (awhen val
         (zerop it)))))

(def every-string-p (object)
  "Return true if OBJECT is a list and all members are strings."
  (∧ (listp object)
     (every #'stringp object)))
