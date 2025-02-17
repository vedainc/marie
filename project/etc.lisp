;;;; -*- mode: lisp; syntax: common-lisp; base: 10; -*-
;;;; etc.lisp --- other commands

(uiop:define-package #:${project}/src/etc
  (:use #:cl
        #:marie
        #:${project}/src/core))

(in-package #:${project}/src/etc)

(define-command nil search (s)
  "search for packages"
  "<package>..."
  t
  (lambda (cmd)
    (let* ((args (clingon:command-arguments cmd))
           (final-args (cons "search" args)))
      (apply #'exe final-args)))
  nil
  "Search for `foo' and `bar'"
  "s foo bar")

(define-command nil date (d)
  "display the date"
  nil
  t
  (lambda (cmd)
    (let* ((args (clingon:command-arguments cmd))
           (final-args (cons "date" args)))
      (exe! final-args)))
  nil
  "Display the current date"
  "d")

(define-command nil zsh-completions (zsh)
  "generate the Zsh completion script"
  ""
  nil
  (lambda (cmd)
    (let ((parent (clingon:command-parent cmd)))
      (clingon:print-documentation :zsh-completions parent t)))
  nil
  "Generate the Zsh completions of ${project} and enable them"
  "zsh-completions > ~/.zsh-completions/_${project}
cat >>! ~/.zshenv << EOF
fpath=(~/.zsh-completions $fpath)
autoload -U compinit
compinit
EOF")

(define-command nil print-doc (doc)
  "print the documentation"
  ""
  nil
  (lambda (cmd)
    (clingon:print-documentation :markdown (clingon:command-parent cmd) t))
  nil
  "Generate the Markdown documentation of ${project} and save it to README.md"
  "print-doc > README.md")
