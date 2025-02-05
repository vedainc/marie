;;;; cli.lisp --- command line interface

(uiop:define-package #:${project}/src/cli
  (:use #:cl
        #:marie
        #:${project}/src/driver))

(in-package #:${project}/src/cli)

(def- cli-opts ()
  "Return the list of options."
  (list
   (clingon:make-option
    :flag
    :description "Display usage"
    :short-name #\h
    :long-name "help"
    :key :help)
   (clingon:make-option
    :string
    :description "Foo"
    :short-name #\f
    :long-name "foo"
    :initial-value "foo-bar-baz"
    :key :foo)))

(def- cli-handler (cmd)
  "The top-level handler function."
  (let ((args (clingon:command-arguments cmd))
        (foo (clingon:getopt cmd :foo)))
    (princ foo)
    (princ (lisp-implementation-type))
    (princ (lisp-implementation-version))))

(def- cli-command ()
  "A command to ${project}."
  (clingon:make-command
   :name "${project}"
   :description "${project}"
   :version (:read-file-form #P"version.sexp")
   :authors '("${author} <${email}>")
   :license ""
   :options (cli-opts)
   :handler #'cli-handler))

(def main ()
  "The main blah blah blah."
  (clingon:run (cli-command)))
