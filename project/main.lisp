;;;; main.lisp --- main entry point

(uiop:define-package #:${project}/src/main
  (:use #:cl
        #:marie
        #:${project}/src/core))

(in-package #:${project}/src/main)


;;; key

(define-command key convert (c)
  "generate a public key for verifying paths"
  "<key>"
  nil
  t
  nil
  "Convert a secret key to a public key"
  "k c foo")

(define-command key generate (g)
  "generate a secret key for signing paths"
  ""
  nil
  t
  nil
  "Generate a new key"
  "k g")

(define-command nil key (k)
  "generate and convert keys"
  "<command>"
  nil
  #'print-usage
  (convert generate))


;;; ext

(define-command nil find (f)
  "find files"
  "<arg>…"
  nil
  (lambda (cmd)
    (let ((args (clingon:command-arguments cmd)))
      (exe! `("fd" ,@args))))
  nil
  "Look for files named `foo'"
  "f foo")

(define-command nil zsh-completions (zsh)
  "generate the Zsh completion script"
  ""
  nil
  (lambda (cmd)
    (let ((parent (clingon:command-parent cmd)))
      (clingon:print-documentation :zsh-completions parent t)))
  nil
  "Generate the Zsh completions of Vix and enable them"
  "zsh-completions > ~/.zsh-completions/_vix
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
  "Generate the Markdown documentation of Vix and save it to README.md"
  "print-doc > README.md")


;;; entry points

(def initialize-system^initialize (&rest args)
  "Initialize the system."
  (values))

(define-main-command
    (("verbose" "verbosity" :counter 0))
    (find
     zsh-completions
     print-doc))

(define-main)
