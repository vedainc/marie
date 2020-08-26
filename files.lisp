;;;; files.lisp

(uiop:define-package #:marie/files
  (:use #:cl
        #:marie/defs))

(in-package #:marie/files)

(def directory-entries (directory)
  "Return top-level files and directories under DIRECTORY."
  (append (uiop:subdirectories directory)
          (uiop:directory-files directory)))

(def collect-entries (list)
  "Return all regular for every directory found under LIST expansion."
  (cond ((or (not (listp list))
             (endp list))
         list)
        ((uiop:directory-exists-p (car list))
         (cons (collect-entries (directory-entries (car list)))
               (collect-entries (cdr list))))
        (t (cons (collect-entries (car list))
                 (collect-entries (cdr list))))))

(def files (pathname)
  "Return all regular files under PATHNAME."
  (marie/sequences:flatten-list (collect-entries (directory-entries pathname))))

(def slurp-file (path)
  "Read entire file as string."
  (uiop:read-file-string path))

(def file-string (path)
  "Read entire file as byte sequence."
  (with-open-file (stream path)
    (let ((val (make-string (file-length stream))))
      (read-sequence val stream)
      val)))

(def resolve-system-file (file system)
  "Return the path of FILE relative to current system."
  (uiop:merge-pathnames* file (asdf:system-source-directory (asdf:find-system system))))
