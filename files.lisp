;;;; files.lisp

(in-package #:mof)

(defun directory-entries (directory)
  "Return top-level files and directories under DIRECTORY"
  (append (uiop:subdirectories directory)
          (uiop:directory-files directory)))

(defun collect-entries (list)
  "Return all regular for every directory found under LIST expansion"
  (cond ((or (not (listp list))
             (endp list))
         list)
        ((uiop:directory-exists-p (car list))
         (cons (collect-entries (directory-entries (car list)))
               (collect-entries (cdr list))))
        (t (cons (collect-entries (car list))
                 (collect-entries (cdr list))))))

(defun files (pathname)
  "Return all regular files under PATHNAME"
  (flatten-list (collect-entries (directory-entries pathname))))
