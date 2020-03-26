;;;; strings.lisp - utilities for working with strings

(uiop:define-package #:marie/strings
  (:use #:cl)
  (:export #:empty-string-p
           #:string-if
           #:cat
           #:cat-intern
           #:string-list
           #:split-string
           #:normalize-strings
           #:trim-whitespace
           #:fmt
           #:fmt*
           #:string-convert
           #:build-string
           #:string-chars))

(in-package #:marie/strings)

(defun empty-string-p (string)
  "Return true if STRING is of length zero."
  (zerop (length string)))

(defun string-if (data)
  "Like IF but returns an empty string when X is false. Otherwise, return DATA."
  (if data data ""))

(defun cat (&rest args)
  "Concatenate ARGS to a string."
  (apply #'concatenate 'string args))

(defun cat-intern (package &rest args)
  "Concatenate ARGS to a string then intern it to the current package."
  (let ((p (if (null package) *package* package)))
    (intern (apply #'cat args) (find-package p))))

(defun string-list (string)
  "Create a list from STRiNG."
  (loop :for char :across string :collect char))

(defun split-string (string char)
  "Split STRING separated by CHAR."
  (loop :for start = 0 :then (1+ finish)
        :for finish = (position char string :start start)
        :collecting (subseq string start finish)
        :until (null finish)))

(defun normalize-strings (list &key (character #\_))
  "Return list of characters with equal length using CHARACTER as end padding."
  (assert (>= (length list) 1))
  (let ((max (apply #'max (mapcar #'length list))))
    (loop :for item :in list
          :for length = (length item)
          :if (= length max) :collect item
          :else
          :collect (cat item (make-string (- max length) :initial-element character)))))

(defun trim-whitespace (string)
  "Trim whitespace characters from STRING."
  (string-trim '(#\Space #\Tab #\Newline #\Linefeed) string))

(defun fmt (&rest args)
  "Simply return a string with FORMAT."
  (apply #'format nil args))

(defun fmt* (&rest args)
  "Print ARGS to stdout with FORMAT."
  (apply #'format t args))

(defun string-convert (value)
  "Convert VALUE to a string."
  (etypecase value
    (number (format nil "~A" value))
    (string value)
    (t (string value))))

(defun build-string (items)
  "Return a string from the concatenation of items."
  (let ((strings (loop :for item :in items :collect (string-convert item))))
    (format nil "~{~A~^ ~}" strings)))

(defun string-chars (string)
  "Return STRING as individual characters."
  (loop :for c :across string :collect c))
