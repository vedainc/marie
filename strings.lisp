;;;; strings.lisp - utilities for working with strings

(uiop:define-package #:marie/strings
  (:use #:cl)
  (:export #:empty-string-p
           #:string*
           #:cat
           #:cat-intern
           #:string-list
           #:split-string
           #:normalize-strings
           #:trim-whitespace
           #:fmt
           #:fmt*
           #:list-string))

(in-package #:marie/strings)

(defun empty-string-p (string)
  "Return true if STRING is of length zero."
  (declare (type (simple-array character (*))  string))
  (zerop (length string)))

(defun string* (value)
  "Return VALUE to a string."
  (etypecase value
    (number (format nil "~A" value))
    (cons (format nil "(~{~A~^ ~})" value))
    (string value)
    (t (string value))))

(defun cat (&rest args)
  "Concatenate ARGS to a string."
  (let ((v (loop :for arg :in args :collect (string* arg))))
    (apply #'concatenate 'string v)))

(defun cat-intern (package &rest args)
  "Concatenate ARGS to a string then intern it to the current package."
  (let ((p (if (null package) *package* package)))
    (intern (apply #'cat args) (find-package p))))

(defun string-list (string)
  "Create a list from STRiNG."
  (declare (type (simple-array character (*))  string))
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

(defun list-string (list)
  "Return the string version of LIST."
  (labels ((fn (args &optional acc)
             (cond ((null args) (string* (nreverse acc)))
                   ((consp (car args))
                    (fn (cdr args)
                        (cons (fn (car args) nil)
                              acc)))
                   (t (fn (cdr args) (cons (car args) acc))))))
    (fn list)))
