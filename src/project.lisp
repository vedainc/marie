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

(eval-always
  (def- run-command (command)
    "Run command around RESTART-CASE."
    (restart-case (string-trim '(#\newline #\tab #\space)
                               (uiop:run-program command :output :string))
      (return-empty-string ()
        "")))

  (def- cmd-output (command)
    "Return the output of running COMMAND as a string."
    (handler-bind ((uiop/run-program:subprocess-error
                     #'(lambda (c)
                         (invoke-restart 'return-empty-string))))
      (run-command command)))

  (defv- *git-user-name* (cmd-output "cd && git config user.name")
    "Preload the Git username")

  (defv- *git-user-email* (cmd-output "cd && git config user.email")
    "Preload the Git user email"))


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
  '(("${project}" . nil)
    ("${PROJECT}" . string-upcase)
    ("${author}"  . git-user-name)
    ("${email}"   . git-user-email))
  "An alist of string substitution where the car is the string to match and the
  cdr is the function to apply.")

(defn- sub-process-error (error)
  ((text :initarg :text :reader text))
  (:documentation "Condition for subprocess errors."))

(def- git-user-name (&rest args)
  "Return the git user name."
  *git-user-name*)

(def- git-user-email (&rest args)
  "Return the git email address."
  *git-user-email*)

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

(def- rep-fmt (string &key project no-header)
  "Return a string with pre-defined substitutions."
  (rep-all (if no-header
               (fmt "~A" string)
               (fmt "~A~%~A" +file-header+ string))
           (or project *project*)))

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
  (rep-fmt "#+title: ${project}
#+author: ${author}
#+email: ${email}
" :no-header t))

(def- make-src-version-stub ()
  "Generate `/version.sexp'."
  (fmt "\"0.0.1\""))

(def- make-src-asdf-stub ()
  "Generate the main ASDF stub."
  (rep-fmt ";;;; ${project}.asd --- top-level ASDF file for ${project}

(defsystem #:${project}
    :name \"${project}\"
    :long-name \"${project}\"
    :description \"\"
    :long-description \"\"
    :version (:read-file-form #P\"version.sexp\")
    :author \"${author} <${email}>\"
    :maintainer \"${author} <${email}>\"
    :license \"\"
    :homepage \"\"
    :bug-tracker \"\"
    :source-control \"\"
    :class :package-inferred-system
    :depends-on (#:marie
                 #:${project}/src/core
                 #:${project}/src/driver
                 #:${project}/src/user)
    :in-order-to ((test-op (test-op \"${project}-tests\"))))
"))

(def- make-src-specials-stub ()
  "Generate `/src/specials.lisp'."
  (rep-fmt ";;;; specials.lisp --- special variables

(uiop:define-package #:${project}/src/specials
  (:use #:cl
        #:marie))

(in-package #:${project}/src/specials)
"))

(def- make-src-utilities-stub ()
  "Generate `/src/utilities.lisp'."
  (rep-fmt ";;;; utilities.lisp --- common utilities

(uiop:define-package #:${project}/src/utilities
  (:use #:cl
        #:marie))

(in-package #:${project}/src/utilities)
"))

(def- make-src-core-stub ()
  "Generate `/src/core.lisp'."
  (rep-fmt ";;;; core.lisp --- core functions

(uiop:define-package #:${project}/src/core
  (:use #:cl
        #:marie))

(in-package #:${project}/src/core)
"))

(def- make-src-driver-stub ()
  "Generate `/src/driver.lisp'."
  (rep-fmt ";;;; driver.lisp --- symbol driver

(uiop:define-package #:${project}/src/driver
  (:nicknames #:${project})
  (:use #:uiop/common-lisp)
  (:use-reexport #:${project}/src/core))

(provide \"${project}\")
(provide \"${PROJECT}\")
"))

(def- make-src-user-stub ()
  "Generate `/src/user.lisp'."
  (rep-fmt ";;;; user.lisp --- user sandbox

(uiop:define-package #:${project}/src/user
  (:nicknames #:${project}-user)
  (:use #:cl
        #:marie
        #:${project}/src/driver))

(in-package #:${project}-user)
"))


;;; tests

(def- make-t-version-stub ()
  "Generate `/version-tests.sexp'."
  (fmt "\"0.0.1\""))

(def- make-t-asdf-stub ()
  "Generate the test ASDF stub."
  (rep-fmt "${project}-tests.asd --- test ASDF file for ${project}

(defsystem #:${project}-tests
    :name \"${project}-tests\"
    :long-name \"${project}\"
    :description \"\"
    :long-description \"\"
    :version (:read-file-form #P\"version-tests.sexp\")
    :author \"${author} <${email}>\"
    :maintainer \"${author} <${email}>\"
    :license \"\"
    :homepage \"\"
    :bug-tracker \"\"
    :source-control \"\"
    :class :package-inferred-system
    :depends-on (#:fiveam
                 #:marie
                 #:${project}
                 #:${project}/t/core-tests
                 #:${project}/t/driver-tests
                 #:${project}/t/user-tests)
    :perform (test-op (o c) (uiop:symbol-call :${project}/t/core-tests :run-tests)))
"))

(def- make-t-core-stub ()
  "Generate `/t/core-tests.lisp'."
  (rep-fmt ";;;; core-tests.lisp -- core functions tests

(uiop:define-package #:${project}/t/core-tests
  (:use #:cl #:marie
        #:fiveam
        #:${project}))

(in-package #:${project}/t/core-tests)

(def run-tests ()
  \"Run all the tests defined in the suite.\"
  (run-all-tests))
")
  )

(def- make-t-driver-stub ()
  "Generate `/t/driver-tests.lisp'."
  (rep-fmt ";;;; driver-tests.lisp --- symbol driver tests

(uiop:define-package :${project}/t/driver-tests
  (:nicknames #:${project}/t)
  (:use #:uiop/common-lisp
        #:marie)
  (:use-reexport #:${project}/t/core-tests))

(provide \"${project}/t\")
(provide \"${PROJECT}/T\")
"))

(def- make-t-user-stub ()
  "Generate `/t/user-tests.lisp'."
  (rep-fmt ";;;; user-tests.lisp --- user sandbox tests

(uiop:define-package :${project}/t/user-tests
  (:nicknames #:${project}-tests-user)
  (:use #:cl #:marie
        #:${project}/t/driver-tests))

(in-package #:${project}-tests-user)
"))


;;;  entrypoints

(def- path (name type)
  "Return a pathname from NAME and TYPE."
  (make-pathname :name name :type type))

(def- %make-project (project &optional (target (home "common-lisp")))
  (let* ((project (normalize-name project))
         (project-dir (build-path project target))
         (project-source-dir (build-path +source-directory+ project-dir))
         (project-tests-dir (build-path +tests-directory+ project-dir)))
    (let ((*project* project))
      (create-directory-structure project-dir)
      (uiop:with-current-directory (project-dir)
        (out-file (path "README" "org") (make-readme-stub))
        (out-file (path project "asd") (make-src-asdf-stub))
        (out-file (path (cat project #\- "tests") "asd") (make-t-asdf-stub))
        (out-file (path "version" "sexp") (make-src-version-stub))
        (out-file (path "version-tests" "sexp") (make-t-version-stub)))
      (uiop:with-current-directory (project-source-dir)
        (out-file (path "core" "lisp") (make-src-core-stub))
        (out-file (path "driver" "lisp") (make-src-driver-stub))
        (out-file (path "user" "lisp") (make-src-user-stub)))
      (uiop:with-current-directory (project-tests-dir)
        (out-file (path "core-tests" "lisp") (make-t-core-stub))
        (out-file (path "driver-tests" "lisp") (make-t-driver-stub))
        (out-file (path "user-tests" "lisp") (make-t-user-stub)))
      (uiop:ensure-directory-pathname project-dir))))

(def- &make-project (&rest args)
  (restart-case (apply #'%make-project args)
    (bail-out ()
      nil)))

(def make-project^mk (&rest args)
  "Create a project skeleton named PROJECT in TARGET."
  (handler-bind ((#+sbcl sb-int:simple-file-error
                  #+lispworks conditions:file-operation-error
                  #-(or sbcl lispworks) error
                  (lambda (c)
                    (invoke-restart 'bail-out))))
    (apply #'&make-project args)))
