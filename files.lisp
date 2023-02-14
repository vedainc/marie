;;;; -*- mode: lisp; syntax: common-lisp; base: 10; coding: utf-8-unix; external-format: (:utf-8 :eol-style :lf); -*-
;;;; files.lisp: utilities for dealing with disk files

(uiop:define-package #:marie/files
  (:use #:cl
        #:marie/definitions
        #:marie/sequences))

(in-package #:marie/files)

(def directory-entries (directory)
  "Return top-level files and directories under DIRECTORY."
  (append (uiop:subdirectories directory)
          (uiop:directory-files directory)))

(def entries·files (list)
  "Return all files for every directory found under LIST expansion."
  (cond ((or (not (listp list))
             (endp list))
         list)
        ((uiop:directory-exists-p (car list))
         (cons (entries (directory-entries (car list)))
               (entries (cdr list))))
        (t (cons (entries (car list))
                 (entries (cdr list))))))

(def read-file-sequence (path)
  "Read entire file as byte sequence."
  (with-open-file (stream path)
    (let ((val (make-string (file-length stream))))
      (read-sequence val stream)
      val)))

(def resolve-system-file (file system)
  "Return the path of FILE relative to SYSTEM."
  (uiop:merge-pathnames* file (asdf:system-source-directory (asdf:find-system system))))

(defm with-output-file ((var path) &body body)
  "A thin wrapper over WITH-OPEN-FILE."
  `(with-open-file (,var ,path
		                     :direction :output
		                     :if-exists :supersede
		                     :if-does-not-exist :create)
     ,@body))
