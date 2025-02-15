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

(defk- +source-directory+
  #P"src"
  "The main source directory.")

(defk- +tests-directory+
  #P"t"
  "The main tests directory.")

(defv- *project-directory*
  (uiop:subpathname (asdf:system-source-directory (asdf:find-system :marie))
                    #P"project/")
  "The location of the project skeleton files.")

(def- project-path (path)
  "Return a path from PATH relevant to the project directory."
  (uiop:subpathname *project-directory* path))

(defv- *project* "project"
  "The default project name.")

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

(def- get-user-name (&rest args)
  "Return the git user name."
  (declare (ignore args))
  (cmd-output "git config user.name || (cd && git config user.name)"))

(def- get-user-email (&rest args)
  "Return the git email address."
  (declare (ignore args))
  (cmd-output "git config user.email || (cd && git config user.email)"))

(defp- *rep-table*
  '(("${project}" . nil)
    ("${PROJECT}" . string-upcase)
    ("${author}"  . get-user-name)
    ("${email}"   . get-user-email))
  "An alist of string substitution where the CAR is the string to match and the
CDR is the function to apply. If the cdr is nil, don't apply any function to
the CAR.")

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

(def- in-file (path &rest args)
  "Like REP-FMT, but read in the contents of PATH, first."
  (let ((path (project-path path)))
    (apply #'rep-fmt (uiop:read-file-string path) (rest args))))

(def- in-file* (path &rest args)
  "Like REP-FMT*, but read in the contents of PATH, first."
  (let ((path (project-path path)))
    (apply #'rep-fmt* (uiop:read-file-string path) (rest args))))

(defn- sub-process-error (error)
  ((text :initarg :text :reader text))
  (:documentation "Condition for subprocess errors."))


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
    ;; root files
    (with-out-files project-dir
      (("README" "org") (in-file* "README.org"))
      (("makefile") (in-file* "makefile"))
      ((".gitignore") (in-file* ".gitignore"))
      (("flake" "nix") (in-file* "flake.nix"))
      (("shells" "nix") (in-file* "shells.nix"))
      (("version" "lisp") (in-file* "version.lisp"))
      (("version-tests" "lisp") (in-file* "version-tests.lisp"))
      ((project "asd") (in-file "project.asd"))
      (((cat project #\- "tests") "asd") (in-file "project-tests.asd")))
    ;; src files
    (with-out-files project-source-dir
      (("core" "lisp") (in-file "core.lisp"))
      (("driver" "lisp") (in-file "driver.lisp"))
      (("user" "lisp") (in-file "user.lisp"))
      (("cli" "lisp") (in-file "cli.lisp"))
      (("build" "lisp") (in-file "build.lisp")))
    ;; test files
    (with-out-files project-tests-dir
      (("core-tests" "lisp") (in-file "core-tests.lisp"))
      (("driver-tests" "lisp") (in-file "driver-tests.lisp"))
      (("user-tests" "lisp") (in-file "user-tests.lisp")))))

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
  "See %MAKE-PROJECT."
  (handler-bind ((#+sbcl sb-int:simple-file-error
                  #+lispworks conditions:file-operation-error
                  #-(or sbcl lispworks) error
                  #'(lambda (c)
                      (declare (ignore c))
                      (invoke-restart 'return-nil))))
    (apply #'&make-project args)))
