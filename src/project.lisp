;;;; -*- mode: lisp; syntax: common-lisp; base: 10; coding: utf-8-unix; external-format: (:utf-8 :eol-style :lf); -*-
;;;; project.lisp ---  create a project tree

(uiop:define-package #:marie/src/project
  (:use #:cl
        #:marie/src/definitions
        #:marie/src/sequences
        #:marie/src/strings
        #:marie/src/etc))

(in-package #:marie/src/project)


;;; variables

(defk- +file-header+
  ";;;; -*- mode: lisp; syntax: common-lisp; base: 10; coding: utf-8-unix; external-format: (:utf-8 :eol-style :lf); -*-")

(defv- *project*
  "project"
  "The default project name.")

(defk- +source-directory+
  #P"src"
  "The main source directory.")

(defk- +tests-directory+
  #P"t"
  "The main tests directory.")

(defv- *version-components*
  '("version" "version-tests")
  "File components for program versions.")

(defv- *code-components*
  '("core" "driver" "user")
  "File components for source code.")


;;; functions

(def- build-path (path1 path2)
  "Return a new path ensuring that PATH2 is a directory."
  (uiop:merge-pathnames* path1 (uiop:ensure-directory-pathname path2)))

(def- create-directory-structure (target)
  "Create the directory structure of the new project under TARGET."
  (let ((directories (list +source-directory+ +tests-directory+)))
    (loop :for dir :in directories
          :for path := (build-path (uiop:ensure-directory-pathname dir) target)
          :do (uiop:ensure-all-directories-exist (list path)))))

(def replace-all (string part replacement &key (test #'char=))
  "Return a new string in which all the occurences of PART in STRING is replaced
 with REPLACEMENT."
  (with-output-to-string (out)
    (loop :with part-length := (length part)
          :for old-pos := 0 :then (+ pos part-length)
          :for pos := (search part string
                              :start2 old-pos
                              :test test)
          :do (write-string string out
                            :start old-pos
                            :end (or pos (length string)))
          :when pos
            :do (write-string replacement out)
          :while pos)))

(defv- *rep-table*
  '(("<>" . nil)
    ("><" . string-upcase)
    ("${author}" . git-user-name)
    ("${email}" . git-user-email))
  "An alist of string substitution where the car is the string to match and the
  cdr is the function to apply.")

(defn- sub-process-error (error)
  ((text :initarg :text :reader text))
  (:documentation "Condition for subprocess errors."))

(def- run-command (command)
  (restart-case (string-trim '(#\newline #\tab #\space)
                             (uiop:run-program command :output :string))
    (return-empty-string ()
      "")))

(def- cmd-output (command)
  "Return the output of running COMMAND as a string."
  (handler-bind ((error
                   #'(lambda (c)
                       (invoke-restart 'return-empty-string))))
    (run-command command)))

(def- git-user-name (&rest args)
  "Return the git user name."
  (cmd-output "cd && git config user.name"))

(def- git-user-email (&rest args)
  "Return the git email address."
  (cmd-output "cd && git config user.email"))

(def- rep-get (string)
  "Return the transformation function for STRING."
  (assoc-value string *rep-table*))

(def- rep-apply (marker string)
  (let* ((fn (rep-get marker))
         (str (if fn (funcall fn string) string)))
    str))

(def- rep (string marker subst)
  "Replace STRING with SUBST."
  (let ((replacement (rep-apply marker subst)))
    (replace-all string marker replacement)))

(def- rep-all (string subst)
  "Perform string replacements blah blah blah."
  (let ((markers (mapcar #'car *rep-table*)))
    (loop :for marker :in markers
          :for str := (rep string marker subst) :then (rep str marker subst)
          :finally (return str))))

(def- rep-fmt (string &optional project)
  "Return a string with pre-defined substitutions."
  (rep-all (fmt "~A
~A" +file-header+ string) (or project *project*)))

(def- out-file (path contents)
  "Generate file in PATH and populate with CONTENTS."
  (with-open-file (out path :direction :output :if-exists :supersede)
    (format out contents)))

(def- make-file (dir name &key (type "lisp") alt)
  "Output the contents of the stub creator, relative to DIR and NAME."
  (let ((stub-name (read-from-string (cat "make" #\- dir #\- name #\- "stub")))
        (out-name (if alt (cat name #\- alt) name)))
    (out-file (make-pathname :name out-name :type type)
              (funcall stub-name))))

(def- normalize-name (name)
  "Return a new string from NAME suitable as a project name."
  (string-downcase (string name)))


;;; src

(def- make-readme-stub ()
  "Generate `/README.org'."
  (rep-fmt "#+title: <>
#+author: ${author}
#+email: ${email}

"))

(def- make-src-version-stub ()
  "Generate `/version.sexp'."
  (fmt "\"0.0.1\""))

(def- make-src-asdf-stub ()
  "Generate the main ASDF stub."
  (rep-fmt ";;;; <>.asd --- top-level ASDF file for <>

(defsystem #:<>
    :name \"<>\"
    :long-name \"<>\"
    :description \"\"
    :long-description \"\"
    :version (:read-file-form #P\"version.sexp\")
    :author \"\"
    :maintainer \"\"
    :license \"\"
    :homepage \"\"
    :bug-tracker \"\"
    :source-control \"\"
    :class :package-inferred-system
    :depends-on (#:marie
                 #:<>/src/core
                 #:<>/src/driver
                 #:<>/src/user)
    :in-order-to ((test-op (test-op \"<>-tests\"))))
"))

(def- make-src-specials-stub ()
  "Generate `/src/specials.lisp'."
  (rep-fmt ";;;; specials.lisp --- special variables

(uiop:define-package #:<>/src/specials
  (:use #:cl
        #:marie))

(in-package #:<>/src/specials)
"))

(def- make-src-utilities-stub ()
  "Generate `/src/utilities.lisp'."
  (rep-fmt ";;;; utilities.lisp --- common utilities

(uiop:define-package #:<>/src/utilities
  (:use #:cl
        #:marie))

(in-package #:<>/src/utilities)
"))

(def- make-src-core-stub ()
  "Generate `/src/core.lisp'."
  (rep-fmt ";;;; core.lisp --- core functions

(uiop:define-package #:<>/src/core
  (:use #:cl
        #:marie))

(in-package #:<>/src/core)
"))

(def- make-src-driver-stub ()
  "Generate `/src/driver.lisp'."
  (rep-fmt ";;;; driver.lisp --- symbol driver

(uiop:define-package #:<>/src/driver
  (:nicknames #:<>)
  (:use #:uiop/common-lisp)
  (:use-reexport #:<>/src/core))

(provide \"<>\")
(provide \"><\")
"))

(def- make-src-user-stub ()
  "Generate `/src/user.lisp'."
  (rep-fmt ";;;; user.lisp --- user sandbox

(uiop:define-package #:<>/src/user
  (:nicknames #:<>-user)
  (:use #:cl
        #:marie
        #:<>/src/driver))

(in-package #:<>-user)
"))


;;; tests

(def- make-t-version-stub ()
  "Generate `/version-tests.sexp'."
  (fmt "\"0.0.1\""))

(def- make-t-asdf-stub ()
  "Generate the test ASDF stub."
  (rep-fmt "<>-tests.asd --- test ASDF file for <>

(defsystem #:<>-tests
    :name \"<>-tests\"
    :long-name \"<>\"
    :description \"\"
    :long-description \"\"
    :version (:read-file-form #P\"version-tests.sexp\")
    :author \"\"
    :maintainer \"\"
    :license \"\"
    :homepage \"\"
    :bug-tracker \"\"
    :source-control \"\"
    :class :package-inferred-system
    :depends-on (#:fiveam
                 #:marie
                 #:<>
                 #:<>/t/core-tests
                 #:<>/t/driver-tests
                 #:<>/t/user-tests)
    :perform (test-op (o c) (uiop:symbol-call :<>/t/core-tests :run-tests)))
"))

(def- make-t-core-stub ()
  "Generate `/t/core-tests.lisp'."
  (rep-fmt ";;;; core-tests.lisp -- core functions tests

(uiop:define-package #:<>/t/core-tests
  (:use #:cl #:marie
        #:fiveam
        #:<>))

(in-package #:<>/t/core-tests)

(def run-tests ()
  \"Run all the tests defined in the suite.\"
  (run-all-tests))
")
  )

(def- make-t-driver-stub ()
  "Generate `/t/driver-tests.lisp'."
  (rep-fmt ";;;; driver-tests.lisp --- symbol driver tests

(uiop:define-package :<>/t/driver-tests
  (:nicknames #:<>/t)
  (:use #:uiop/common-lisp
        #:marie)
  (:use-reexport #:<>/t/core-tests))

(provide \"<>/t\")
(provide \"></T\")
"))

(def- make-t-user-stub ()
  "Generate `/t/user-tests.lisp'."
  (rep-fmt ";;;; user-tests.lisp --- user sandbox tests

(uiop:define-package :<>/t/user-tests
  (:nicknames #:<>-tests-user)
  (:use #:cl #:marie
        #:<>/t/driver-tests))

(in-package #:<>-tests-user)
"))


;;;  entrypoints

(def make-project^mk (project &optional (target (home "common-lisp")))
  "Create a project skeleton named PROJECT in TARGET."
  (let* ((project (normalize-name project))
         (project-dir (build-path project target))
         (project-source-dir (build-path +source-directory+ project-dir))
         (project-tests-dir (build-path +tests-directory+ project-dir)))
    (handler-bind ((#+sbcl sb-int:simple-file-error
                    #+lispworks conditions:file-operation-error
                    #-(or sbcl lispworks) error
                    (lambda (c)
                      (format t "Filesystem error: ~A~%" c)
                      (uiop:quit))))
      (let ((*project* project))
        (create-directory-structure project-dir)
        (uiop:with-current-directory (project-dir)
          (out-file (make-pathname :name "README" :type "org") (make-readme-stub))
          (out-file (make-pathname :name project :type "asd") (make-src-asdf-stub))
          (out-file (make-pathname :name (cat project #\- "tests") :type "asd") (make-t-asdf-stub))
          (make-file "src" "version" :type "sexp")
          (make-file "t" "version" :type "sexp" :alt "tests"))
        (uiop:with-current-directory (project-source-dir)
          (make-file "src" "core")
          (make-file "src" "driver")
          (make-file "src" "user"))
        (uiop:with-current-directory (project-tests-dir)
          (make-file "t" "core" :alt "tests")
          (make-file "t" "driver" :alt "tests")
          (make-file "t" "user" :alt "tests"))))))
