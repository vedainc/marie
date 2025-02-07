;;;; -*- mode: lisp; syntax: common-lisp; base: 10; coding: utf-8-unix; external-format: (:utf-8 :eol-style :lf); -*-
;;;; filesystem.lisp --- utilities for dealing with disk files and filesystem

(uiop:define-package #:marie/src/filesystem
  (:use #:cl
        #:marie/src/definitions
        #:marie/src/sequences
        #:marie/src/strings))

(in-package #:marie/src/filesystem)


;;; File directory related fns

(def directory-entries (directory)
  "Return top-level files and directories under DIRECTORY."
  (append (uiop:subdirectories directory)
          (uiop:directory-files directory)))

(def entries^files (list)
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

(def home^~ (path)
  "Return a path relative to the home directory."
  ;; (uiop:merge-pathnames* path (user-homedir-pathname))
  ;; (merge-pathnames path (user-homedir-pathname))
  (uiop:subpathname (user-homedir-pathname) path))

(def expand-pathname (path)
  "Return a path while performing tilde expansion."
  (let ((home (uiop:pathname-parent-directory-pathname (user-homedir-pathname)))
        (pathstring (uiop:native-namestring path)))
    (cond ((and (char-equal (elt pathstring 0) #\~)
                (char-equal (elt pathstring 1) #\/))
           (home (subseq pathstring 2)))
          ((and (char-equal (elt pathstring 0) #\~)
                (not (char-equal (elt pathstring 1) #\/)))
           (uiop:subpathname home (subseq pathstring 1)))
          (t (uiop:ensure-absolute-pathname pathstring)))))

(def read-integer (string)
  "Return integer from STRING."
  (parse-integer string :junk-allowed t))

(def read-integer-line (file)
  "Return integer from a line in FILE."
  (read-integer (read-line file nil)))

(def display-file (file)
  "Display the contents of FILE."
  (let ((in (open file :if-does-not-exist nil)))
    (when in
      (loop :for line = (read-line in nil)
            :while line
            :do (format t "~A~%" line))
      (close in))))


;;; System fns

(def system-object^sys-object (system)
  "Return the system object for the current system."
  (asdf:find-system system))

(def system-path^sys-path (system)
  "Return the ASDF file path for the current system."
  (let ((object (system-object system)))
    (uiop:merge-pathnames* (cat system ".asd")
                           (asdf:system-source-directory object))))

(def system-version^sys-version (system)
  "Return the version number extracted from the system resources."
  (asdf:system-version (sys-object system)))
