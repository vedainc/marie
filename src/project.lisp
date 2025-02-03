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

(defv- *project*
  "project"
  "The default project name.")

(defk- *source-directory*
  #P"src"
  "The main source directory.")

(defk- *tests-directory*
  #P"t"
  "The main tests directory.")

(defk- *file-header*
  ";;;; -*- mode: lisp; syntax: common-lisp; base: 10; coding: utf-8-unix; external-format: (:utf-8 :eol-style :lf); -*-")


;;; functions

(def- create-directory-structure (target)
  "Create the directory structure of the new project under TARGET."
  (let ((directories (list *source-directory* *tests-directory*)))
    (loop :for dir :in directories
          :for path := (uiop:merge-pathnames* dir target)
          :do (uiop:ensure-all-directories-exist path))))

(def- replace-all (string part replacement &key (test #'char=))
  "Return a new string from STRING in which all the occurences of PART
is replaced with REPLACEMENT."
  (with-output-to-string (out)
    (loop :with part-length := (length part)
          :for old-pos := 0 :then (+ pos part-length)
          :for pos := (search part string
                              :start2 old-pos
                              :test test)
          :do (write-string string out
                            :start old-pos
                            :end (or pos (length string)))
          :when pos :do (write-string replacement out)
            :while pos)))

(def- rep (string project)
  "Replace STRING with PROJECT."
  (replace-all string "<>" project))

(def- rep-fmt (string &optional project)
  "Return a string with pre-defined substitutions."
  (rep (fmt "~A
~A" +file-header+ string) (or project *project*)))


;;; src

(def- make-src-version-stub ()
  "Generate `/version.sexp'."
  (fmt "\"0.0.1\""))

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
(provide (string-upcase \"<>\"))
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

(def- make-version-tests-stub ()
  "Generate `/version-tests.sexp'."
  (fmt "\"0.0.1\""))

(def- make-src-asdf-stub ()
  "Generate the main ASDF stub."
  (rep-fmt ";;;; <>.asd --- top-level ASDF file for <>

(defsystem #:<>
    :name \"<>\"
    :version (:read-file-form #P\"version.sexp\")
    :description \"\"
    :long-description \"\"
    :author \"\"
    :class :package-inferred-system
    :depends-on (#:marie
                 #:<>/src/core
                 #:<>/src/driver
                 #:<>/src/user)
    :in-order-to ((test-op (test-op \"<>-tests\"))))
"))

(def- make-tests-asdf-stub ()
  "Generate the test ASDF stub."
  (rep-fmt "<>-tests.asd --- test ASDF file for <>

(defsystem #:<>-tests
    :name \"<>-tests\"
    :version (:read-file-form #P\"version-tests.sexp\")
    :description \"\"
    :author \"\"
    :class :package-inferred-system
    :depends-on (#:fiveam
                 #:marie
                 #:<>
                 #:<>/t/core-tests
                 #:<>/t/driver-tests
                 #:<>/t/user-tests)
    :perform (test-op (o c) (uiop:symbol-call :<>/t/core-tests :run-tests)))
"))

(def- make-core-tests-stub ()
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

(def- make-driver-tests-stub ()
  "Generate `/t/driver-tests.lisp'."
  (rep-fmt ";;;; driver-tests.lisp --- symbol driver tests

(uiop:define-package :<>/t/driver-tests
  (:nicknames #:<>/t)
  (:use #:uiop/common-lisp
        #:marie)
  (:use-reexport #:<>/t/core-tests))

(provide \"<>/t\")
(provide \"<>/T\")
"))

(def- make-driver-user-stub ()
  "Generate `/t/user-tests.lisp'."
  (rep-fmt ";;;; user-tests.lisp --- user sandbox tests

(uiop:define-package :<>/t/user-tests
  (:nicknames #:<>-tests-user)
  (:use #:cl #:marie #:meria
        #:<>/t/driver-tests))

(in-package #:<>-tests-user)
"))


;;;  entrypoints

(def- out-file (path contents)
  "Generate file in PATH and populate with CONTENTS."
  (let ((p (if (string= "." path) (uiop:getcwd) path)))
    (with-open-file (out p :direction :output :if-exists :supersede)
      (format out contents))))

(defm- make-file (dir name &optional (type "lisp"))
  "Output the contents of the stub creator, relative to DIR and NAME."
  (let ((stub-name (read-from-string (cat "make" #\- dir #\- name #\- "stub"))))
    `(out-file (make-pathname :name ,name :type ,type) (,stub-name))))

(def make-project^mk (project &optional target)
  "Create a project skeleton named PROJECT in TARGET."
  (let* ((base (or target (home "common-lisp")))
         (project-dir (uiop:merge-pathnames* project base)))
    ;; (handler-bind ...)
    (let ((*project* project))
      (create-directory-structure project-dir)
      (uiop:with-current-directory ((project-dir))
        (out-file #P"version.sexp" (make-version-stub))
        (out-file #P"version-tests.sexp" (make-version-tests-stub)))
      (uiop:with-current-directory ((uiop:merge-pathnames* *source-directory* project-dir))
        (make-file "src" "core")
        (make-file "src" "driver")
        (make-file "src" "user"))
      (uiop:with-current-directory ((uiop:merge-pathnames* *tests-directory* project-dir))
        (make-file "t" "core-tests")
        (make-file "t" "driver-tests")
        (make-file "t" "user-tests")))))
