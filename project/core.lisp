;;;; core.lisp --- core CLI functions

(uiop:define-package #:${project}/src/core
  (:use #:cl
        #:marie
        #:${project}/src/specials))

(in-package #:${project}/src/core)


;;; run stuff

(def exe! (cmd)
  "Run CMD."
  (uiop:run-program cmd :input :interactive :output :interactive :error-output :interactive))

(def exe (&rest args)
  "Use the main program command to run ARGS."
  (exe! (append +exe+ (flatten-list args))))


;;; common fns

(def or-args (args)
  "Return a string separated by #\|, from the list ARGS."
  (format nil "~{~A~^|~}" args))
(def mini-help (&rest args)
  "Return a list suitable for the example usage of a command."
  (let ((parts (partition args 2)))
    (loop :for (desc usage) :in parts
          :collect (cons (fmt "~A:" desc) (fmt "~A ~A" +project-name+ usage)))))

(def cmd-handler (cmd fn)
  "Define a function that parses the command arguments from CMD and runs the
command CMD from it."
  (let ((args (clingon:command-arguments cmd)))
    (funcall fn args)))

(def- prefix-command (prefix cmd)
  "Return a prefix string for CMD if PREFIX is true."
  (let ((prefix-string (if prefix (prin1-to-string prefix) "")))
    (if (¬ (empty-string-p prefix-string))
        (cat prefix-string "/" cmd)
        cmd)))

(defm make-opt (name desc type value &rest rest)
  "Return an option object from NAME."
  (let* ((%name (eval name))
         (%char (aref %name 0))
         (%opt-name (read-cat ":" "opt-" %name))
         (%desc (if desc desc (fmt "use the `~A' option" %name))))
    `(clingon:make-option
      ,type
      :description ,%desc
      :short-name ,%char
      :long-name ,%name
      :required nil
      :key ,%opt-name
      :initial-value ,value
      ,@rest)))

(def- split-name (symbol &key (separator '(#\^)))
  "Return the split of SYMBOL by SEPARATOR."
  (uiop:split-string (prin1-to-string symbol) :separator separator))

(def- get-raw-name (symbol)
  (destructuring-bind (main-name &optional alt-name)
      (split-name symbol)
    (declare (ignore alt-name))
    (read-from-string main-name)))

(def- get-name (symbol)
  (destructuring-bind (main-name &optional alt-name)
      (split-name symbol)
    (declare (ignorable alt-name))
    (if alt-name
        (read-from-string alt-name)
        (read-from-string main-name))))

(def print-usage (cmd)
  "Print usage of CMD then exit."
  (clingon:print-usage-and-exit cmd t))


;;; command generators

(defm define-options (group command &rest args)
  "Define a function that returns the options for COMMAND."
  (flet ((prefix (command)
           (prefix-command group command)))
    (let ((%fn (read-cat (prefix command) '/options))
          (%doc (fmt "Return the options for the `~A' command." command)))
      `(def- ,%fn ()
         ,%doc
         (append (list) ,@args)))))

(defm define-handler (group command command-list)
  "Define a handler for COMMAND."
  (flet ((prefix (command)
           (prefix-command group command)))
    (let ((%fn (read-cat (prefix command) '/handler))
          (%doc (fmt "The handler for the `~A' command." command)))
      `(def- ,%fn (cmd)
         ,%doc
         (let* ((args (clingon:command-arguments cmd))
                (final-args (append ',command-list args)))
           (apply #'exe final-args))))))

(defm define-basic-handler (group command)
  "Define a basic handler for handling COMMAND."
  (flet ((prefix (command)
           (prefix-command group command)))
    (let ((%fn (read-cat (prefix command) '/handler))
          (%doc (fmt "The basic handler for the `~A' command." command)))
      `(def- ,%fn (cmd)
         ,%doc
         (if (null args)
             (print-usage cmd)
             (apply #'exe final-args))))))

(defm define-sub-commands (name &rest sub-commands)
  "Return a list of subcommands for NAME from ARGS."
  (let ((%name (read-cat name '/sub-commands)))
    `(def- ,%name ()
       (list
        ,@(loop :for command :in sub-commands
                :for fname := (read-cat name #\/ command '/command)
                :collect `(,fname))))))

(defm define-command (group command aliases
                      description
                      usage
                      options
                      handler
                      sub-commands
                      &rest examples)
  "Define a function for CLINGON:MAKE-COMMAND.

GROUP is the name of the group, like `flake'.

COMMAND is the name of the command from a group, or standalone, like `init'.

ALIASES is a list of alternative names, like `i' for `init'.

DESCRIPTION is the command description that will show up in help.

OPTIONS is the name of the function, that will be called to the program options;
or T, which means to generate a default set of options.

HANDLER is a function object that accepts a single argument that will handle the
command itself; T to define a handler that accepts a group and (sub) command; or
NIL to define a basic handler that acts like T, but without the group parameter.

SUB-COMMANDS is T, to indicate that the command being defined will handle
sub-commands.

EXAMPLES is a list of description & command-line usage pairs for the command.
"
  (flet ((prefix (command)
           (prefix-command group command)))
    (let* ((%group (when group (prin1-downcase group)))
           (%command-raw (prin1-downcase (get-raw-name command)))
           (%command (prin1-downcase (get-name command)))
           (%aliases (when aliases (mapcar #'prin1-downcase aliases)))
           (%fn (read-cat (prefix %command) '/command))
           (%options (read-cat (prefix %command) '/options))
           (%handler (read-cat "#'" (prefix %command) '/handler))
           (%sub-commands (read-cat (prefix %command) '/sub-commands)))
      `(progn
         ,(when (eql t options) `(define-options ,group ,command))
         ,(cond
            ;; commands with groups
            ((eql t handler)
             `(define-handler ,group ,command (,%group ,%command-raw)))
            ;; commands without groups
            ((null handler)
             `(define-basic-handler ,group ,command))
            ;; use the specified handler
            (t nil))
         ,(when (listp sub-commands)
            `(define-sub-commands ,command ,@sub-commands))
         (def ,%fn ()
           (clingon:make-command
            :name ,%command
            :aliases ',%aliases
            :description ,description
            :usage ,(if (null usage) "[<argument>...|<option>...]" usage)
            :options ,(cond ((eql t options)
                             `(,%options))
                            (t options))
            :handler ,(cond ((∨ (eql t handler) (null handler))
                             %handler)
                            (t handler))
            ,@(when sub-commands `(:sub-commands (,%sub-commands)))
            ,@(when examples `(:examples (mini-help ,@examples)))))))))


;;; main generators

(defm define-main-options (name options)
  "Define a function to return the list of options for the main command."
  (let* ((%name (prin1-downcase name))
         (%fn (read-cat %name '/options)))
    `(def ,%fn ()
       "Return the options for the main command."
       (list
        ,@(loop :for option :in options :collect `(make-opt ,@option))))))

(defm define-main-sub-commands (name commands)
  "Define a function to define the sub-commands of main."
  (let* ((%name (prin1-downcase name))
         (%fn (read-cat %name '/sub-commands)))
    `(def ,%fn ()
       (list
        ,@(loop :for command :in commands
                :for command-name := (read-cat command '/command)
                :collect `(,command-name))))))

(defm define-main-command (options sub-commands)
  "Define a function as the main command."
  (let* ((%name "main")
         (%fn (read-cat %name '/command))
         (%options (read-cat %name '/options))
         (%sub-commands (read-cat %name '/sub-commands)))
    `(progn
       (define-main-options main ,options)
       (define-main-sub-commands main ,sub-commands)
       (def ,%fn ()
         "Define the main command"
         (clingon:make-command
          :name ,+project-name+
          :version ,+project-version+
          :description #.(uiop:read-file-form (make-pathname :directory '(:relative "src") :name "version" :type "lisp"))
          :options (,%options)
          :handler #'print-usage
          :sub-commands (,%sub-commands))))))

(defm define-main ()
  "Define the main entry point function."
  (let* ((%name "main")
         (%command (read-cat %name '/command)))
    `(def main^${project} (&rest args)
       "The main entry point of the program."
       (declare (ignorable args))
       (let ((app (,%command)))
         (handler-case (clingon:run app)
           (#+sbcl sb-sys:interactive-interrupt
            #+lispworks mp:process-interrupt
            #+ecl ext:interactive-interrupt
            #+ccl ccl:interrupt-signal-condition
            #+clisp system::simple-interrupt-condition
            #+allegro excl:interrupt-signal
            () nil)
           (error (c)
             (format t "Oof, an unknown error occured:~&~A~&" c)))))))
