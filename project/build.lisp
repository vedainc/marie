;;; build.lisp --- build the ${project} executable

(require 'asdf)
(defun cwd-name ()
  (multiple-value-bind (type list &rest rest)
      (uiop:split-unix-namestring-directory-components
       (namestring (uiop:getcwd)))
    (declare (ignore rest))
    (car (last list))))
(defun cwd-keyword () (intern (cwd-name) (find-package :keyword)))
(defun home (path) (merge-pathnames path (user-homedir-pathname)))
#-quicklisp (load (home "quicklisp/setup.lisp"))
(push (uiop:getcwd) asdf:*central-registry*)
(ql:quickload (cwd-keyword))
(asdf:load-system (cwd-keyword))
(asdf:make (cwd-keyword))
