;;;; strings.lisp

;;; Utilities for working with strings

(in-package :mof)

(defun digest-string (string &optional (type :sha256))
  "Return a string of a digest of a string."
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    type
    (#+SBCL sb-ext:string-to-octets
     #+CCL ccl:encode-string-to-octets
     #+CLISP ext:convert-string-to-bytes
     #+CMUCL stream:string-to-octets
     #+CLISP charset:utf-8
     string))))

(defun string-if (data)
  "Like IF but returns an empty string when X is false. Otherwise, return DATA."
  (if data data ""))

(defun cat (&rest args)
  "Concatenate ARGS to a string"
  (apply #'concatenate 'string args))

(defun string-list (string)
  "Create a list from STRiNG"
  (loop :for char :across string :collect char))

(defun split-string (string char)
  "Split STRING separated by CHAR"
  (loop :for start = 0 :then (1+ finish)
     :for finish = (position char string :start start)
     :collecting (subseq string start finish)
     :until (null finish)))

(defun join-strings (list)
  "Merge strings in LIST by the space character"
  (format nil "~{~A~^ ~}" list))

(defun normalize-strings (list &key (character #\_))
  "Return list of characters with equal lengthn using CHARACTER as end padding"
  (assert (>= (length list) 1))
  (let ((max (apply #'max (mapcar #'length list))))
    (loop
       :for item :in list
       :for length = (length item)
       :if (= length max) :collect item
       :else
       :collect (cat item (make-string (- max length) :initial-element character)))))

