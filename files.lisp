;;;; files.lisp - file and path utilities

(uiop:define-package #:marie/files
  (:use #:cl)
  (:export #:files
           #:slurp-file
           #:file-string
           #:resolve-system-file))

(in-package #:marie/files)

(defun directory-entries (directory)
  "Return top-level files and directories under DIRECTORY."
  (append (uiop:subdirectories directory)
          (uiop:directory-files directory)))

(defun collect-entries (list)
  "Return all regular for every directory found under LIST expansion."
  (cond ((or (not (listp list))
             (endp list))
         list)
        ((uiop:directory-exists-p (car list))
         (cons (collect-entries (directory-entries (car list)))
               (collect-entries (cdr list))))
        (t (cons (collect-entries (car list))
                 (collect-entries (cdr list))))))

(defun files (pathname)
  "Return all regular files under PATHNAME."
  (marie/sequences:flatten-list (collect-entries (directory-entries pathname))))

(defun slurp-file (path)
  "Read entire file as string."
  (uiop:read-file-string path))

(defun file-string (path)
  "Read entire file as byte sequence."
  (with-open-file (stream path)
    (let ((val (make-string (file-length stream))))
      (read-sequence val stream)
      val)))

(defun resolve-system-file (file system)
  "Return the path of FILE relative to current system."
  (uiop:merge-pathnames* file (asdf:system-source-directory (asdf:find-system system))))
