;;;; cli.lisp --- command line interface

(uiop:define-package #:${project}/src/cli
  (:use #:cl
        #:marie))

(in-package #:${project}/src/cli)

(defk- +project-name+
  "${project}"
  "The name of the project.")

(defk- +project-version+
  "0.0.0"
  "The version number of the project.")

(def- cli-opts ()
  "Return the list of options."
  (list
   (clingon:make-option
    :flag
    :description "Display short help"
    :short-name #\h
    :key :help)
   (clingon:make-option
    :string
    :description "Project name"
    :short-name #\p
    :long-name "project"
    :initial-value +project-name+
    :key :project)))

(def- cli-handler (cmd)
  "The top-level handler function."
  (let ((args (clingon:command-arguments cmd))
        (project (clingon:getopt cmd :project)))
    (princ project)
    (terpri)
    (princ +project-version+)
    (terpri)))

(def- cli-command ()
  "A command to ${project}."
  (clingon:make-command
   :name "${project}"
   :description "${project}"
   :version +project-version+
   :authors '("${author} <${email}>")
   :license ""
   :options (cli-opts)
   :handler #'cli-handler))

(def main ()
  "The main blah blah blah."
  (clingon:run (cli-command)))
