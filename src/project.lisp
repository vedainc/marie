;;;; -*- mode: lisp; syntax: common-lisp; base: 10; coding: utf-8-unix; external-format: (:utf-8 :eol-style :lf); -*-
;;;; project.lisp ---  create a project tree

(uiop:define-package #:marie/src/project
  (:use #:cl
        #:marie/src/definitions
        #:marie/src/sequences
        #:marie/src/strings
        #:marie/src/etc
        #:marie/src/filesystem))

(in-package #:marie/src/project)


;;; t

(defk- +file-header+
  ";;;; -*- mode: lisp; syntax: common-lisp; base: 10; -*-")

(defv- *project* "project"
  "The default project name.")

(defk- +source-directory+
  #P"src"
  "The main source directory.")

(defk- +tests-directory+
  #P"t"
  "The main tests directory.")

(def- run-trim (command)
  "Run COMMAND and remove trailing whitespace."
  (string-trim '(#\newline #\tab #\space) (uiop:run-program command :output :string)))

(def- &cmd-output (command)
  "Run command around RESTART-CASE."
  (restart-case (run-trim command)
    (return-empty-string ()
      "")))

(def- cmd-output (command)
  "Return the output of running COMMAND as a string."
  (handler-bind ((uiop/run-program:subprocess-error
                   #'(lambda (c)
                       (declare (ignore c))
                       (invoke-restart 'return-empty-string))))
    (&cmd-output command)))

;; allow reloading
(defp- *git-user-name* (cmd-output "git config user.name || (cd && git config user.name)")
  "Preload the Git username")

(defp- *git-user-email* (cmd-output "git config user.email || (cd && git config user.email)")
  "Preload the Git user email")


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
  (declare (ignore args))
  *git-user-name*)

(def- git-user-email (&rest args)
  "Return the git email address."
  (declare (ignore args))
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

(def- rep-fmt* (&rest args)
  "Like, REP-FMT, but without HEADERS."
  (apply #'rep-fmt (append args (list :no-header t))))


;;; src

(def- make-readme-stub ()
  "Generate `/README.org'."
  (rep-fmt* "#+title: ${project}
#+author: ${author}
#+email: ${email}
"))

(def- make-gitignore-stub ()
  "Generate `/.gitignore'."
  (rep-fmt* "*.fasl
*.64yfasl
*.lisp-temp
*.dfsl
*.pfsl
*.d64fsl
*.p64fsl
*.lx64fsl
*.lx32fsl
*.dx64fsl
*.dx32fsl
*.fx64fsl
*.fx32fsl
*.sx64fsl
*.sx32fsl
*.wx64fsl
*.wx32fsl
"))

(def- make-makefile-stub ()
  "Generate `/makefile'."
  (rep-fmt* "SHELL := bash
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules

.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c
.DELETE_ON_ERROR:

NAME = ${project}
LISP := sbcl

.PHONY: all $(NAME) clean

all: $(NAME)

$(NAME):
	@(LISP) --eval '(ql:quickload :${project})' --eval '(asdf:make :${project})' --eval '(uiop:quit)'

clean:
	@rm -f $(NAME)
"))

(def- make-flake-nix-stub ()
  "Generate `flake.nix'."
  (rep-fmt* "{
  description = \"A flake\";
  inputs = {
    nixpkgs.url = \"github:nixos/nixpkgs/nixpkgs-unstable\";
    flake-utils.url = \"github:numtide/flake-utils\";
  };
  outputs = { nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.system;
      in { devShells = import ./shells.nix { inherit nixpkgs pkgs; }; });
}
"))

(def- make-shells-nix-stub ()
  "Generate `shells.nix'."
  (rep-fmt* "{ nixpkgs, pkgs, ... }:
with pkgs; rec {
  lisp = mkShell { buildInputs = [ sbcl ecl ]; };
  default = lisp;
}
"))

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
                 #:clingon
                 #:${project}/src/core
                 #:${project}/src/driver
                 #:${project}/src/user
                 #:${project}/src/cli)
    :in-order-to ((test-op (test-op \"${project}-tests\"))))
    :build-operation \"program-op\"
    :build-pathname \"${project}\"
    :entry-point \"${project}/src/cli:main\"
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

(def main^hello ()
  \"Display a greeting.\"
  (format t \"Hello, world!~%\"))
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

(def- make-src-cli-stub ()
  "Generate `/src/cli.lisp'."
  (rep-fmt ";;;; cli.lisp --- command line interface

(uiop:define-package #:${project}/src/cli
  (:use #:cl
        #:marie
        #:${project}/src/driver))

(in-package #:${project}/src/cli)

(def- cli-opts ()
  \"Return the list of options.\"
  (list
   (clingon:make-option
    :flag
    :description \"Display usage\"
    :short-name #\\h
    :long-name \"help\"
    :key :help)
   (clingon:make-option
    :string
    :description \"Foo\"
    :short-name #\\f
    :long-name \"foo\"
    :initial-value \"foo-bar-baz\"
    :key :foo)))

(def- cli-handler (cmd)
  \"The top-level handler function.\"
  (let ((args (clingon:command-arguments cmd))
        (foo (clingon:getopt cmd :foo)))
    (princ foo)
    (princ (lisp-implementation-type))
    (princ (lisp-implementation-version))))

(def- cli-command ()
  \"A command to ${project}.\"
  (clingon:make-command
   :name \"${project}\"
   :description \"${project}\"
   :version (:read-file-form #P\"version.sexp\")
   :authors '(\"${author} <${email}>\")
   :license \"\"
   :options (cli-opts)
   :handler #'cli-handler))

(def main ()
  \"The main blah blah blah.\"
  (clingon:run (cli-command)))
"))

(def- make-src-build-stub ()
  "Generate `/src/build.lisp'."
  (rep-fmt "(require 'asdf)
(defun cwd-name ()
  (multiple-value-bind (type list &rest rest)
      (uiop:split-unix-namestring-directory-components
       (namestring (uiop:getcwd)))
    (car (last list))))
(defun cwd-keyword () (intern (cwd-name) (find-package :keyword)))
(defun home (path) (merge-pathnames path (user-homedir-pathname)))
#-quicklisp (load (home \"quicklisp/setup.lisp\"))
(push (uiop:getcwd) asdf:*central-registry*)
(ql:quickload (cwd-keyword))
(asdf:make (cwd-keyword))
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


;;;  helpers

(defm- with-out-files (dir &body file-specs)
  "Define macro helper to avoid out-file repetition."
  `(uiop:with-current-directory (,dir)
     ,@(loop :for (fname contents) :in file-specs
             :collect `(out-file (path ,@fname) ,contents))))

(def- path (name &optional type)
  "Return a pathname from NAME and TYPE."
  (make-pathname :name name :type type))

(def- normalize-name (name)
  "Return a new string from NAME suitable as a project name."
  (string-downcase (string name)))

(def- out-file (path contents)
  "Generate file in PATH and populate with CONTENTS."
  (with-open-file (out path :direction :output :if-exists :supersede)
    (format out contents)))

(def- out-files (project project-dir)
  "Write the project files in PROJECT-DIR."
  (let ((project-source-dir (build-path +source-directory+ project-dir))
        (project-tests-dir (build-path +tests-directory+ project-dir)))
    ;; Root files
    (with-out-files project-dir
      (("README" "org") (make-readme-stub))
      (("makefile") (make-makefile-stub))
      ((".gitignore") (make-gitignore-stub))
      (("flake" "nix") (make-flake-nix-stub))
      (("shells" "nix") (make-shells-nix-stub))
      ((project "asd") (make-src-asdf-stub))
      (((cat project #\- "tests") "asd") (make-t-asdf-stub))
      (("version" "sexp") (make-src-version-stub))
      (("version-tests" "sexp") (make-t-version-stub)))
    ;; src files
    (with-out-files project-source-dir
      (("core" "lisp") (make-src-core-stub))
      (("driver" "lisp") (make-src-driver-stub))
      (("user" "lisp") (make-src-user-stub))
      (("cli" "lisp") (make-src-cli-stub))
      (("build" "lisp") (make-src-build-stub)))
    ;; test files
    (with-out-files project-tests-dir
      (("core-tests" "lisp") (make-t-core-stub))
      (("driver-tests" "lisp") (make-t-driver-stub))
      (("user-tests" "lisp") (make-t-user-stub)))))

(def- enroll-system (project-dir)
  "Make the system indicated by PROJECT-DIR immediately accessible by ASDF."
  (push (uiop:ensure-directory-pathname project-dir) asdf:*central-registry*)
  (uiop:ensure-directory-pathname project-dir))


;;; entry points

(def- %make-project (project &key (target (home "common-lisp")))
  "Create a project skeleton named PROJECT in TARGET."
  (let ((project (normalize-name project)))
    (unless (empty-string-p project)
      (let ((project-dir (build-path project target))
            (*project* project))
        (create-directory-structure project-dir)
        (out-files project project-dir)
        (enroll-system project-dir)))))

(def- &make-project (&rest args)
  "See %MAKE-PROJECT."
  (restart-case (apply #'%make-project args)
    (return-nil ()
      nil)))

(def make-project^mk (&rest args)
  "See %MAKE-PROJECT.
  (mk \"foo\")"
  (handler-bind ((#+sbcl sb-int:simple-file-error
                  #+lispworks conditions:file-operation-error
                  #-(or sbcl lispworks) error
                  #'(lambda (c)
                      (declare (ignore c))
                      (invoke-restart 'return-nil))))
    (apply #'&make-project args)))
