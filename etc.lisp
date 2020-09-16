;;;; etc.lisp

(uiop:define-package #:marie/etc
  (:use #:cl
        #:marie/defs))

(in-package #:marie/etc)

(def apropos* (&rest args)
  "Display sorted matching symbols from ARGS with CL:APROPOS."
  (loop :for symbol :in (sort (apply #'apropos-list args) #'string<)
        :do (format t "~S~%" symbol)))

(def read-integer (string)
  "Return integer from STRING."
  (parse-integer string :junk-allowed t))

(def read-integer-line (file)
  "Return integer from a line in FILE."
  (read-integer (read-line file nil)))

(def display-file (file)
  "Display the contents of FILE."
  (let ((in (open file :if-does-not-exist nil)))
    (when in
      (loop :for line = (read-line in nil)
            :while line
            :do (format t "~A~%" line))
      (close in))))

(def collect-characters (start end)
  "Collect ASCII characters from START to END."
  (loop :for index :from start :below (+ start end) :collect (code-char index)))

(def copy-table (hash-table)
  "Return a new hash table from HASH-TABLE."
  (let ((table (make-hash-table :test (hash-table-test hash-table)
                                :rehash-size (hash-table-rehash-size hash-table)
                                :rehash-threshold (hash-table-rehash-threshold hash-table)
                                :size (hash-table-size hash-table))))
    (loop :for key :being each hash-key :of hash-table
          :using (hash-value value)
          :do (setf (gethash key table) value)
          :finally (return table))))

(def (home ~) (path)
  "Return a path relative to the home directory."
  (uiop:subpathname (user-homedir-pathname) path))

(def expand-pathname (path)
  "Return a path while performing tilde expansion."
  (let ((home (uiop:pathname-parent-directory-pathname (user-homedir-pathname)))
        (pathstring (uiop:native-namestring path)))
    (cond ((and (char-equal (elt pathstring 0) #\~)
                (char-equal (elt pathstring 1) #\/))
           (home (subseq pathstring 2)))
          ((and (char-equal (elt pathstring 0) #\~)
                (not (char-equal (elt pathstring 1) #\/)))
           (uiop:subpathname home (subseq pathstring 1)))
          (t (uiop:ensure-absolute-pathname pathstring)))))

(def make (system)
  "Use ASDF to load SYSTEM forcibly. "
  (asdf:make system :force t))

(defm with-time ((&optional) &body body)
  "Execute BODY then return timing information."
  `(time (progn ,@body (values))))

(def true (&rest args)
  "Return true for anything."
  (declare (ignore args))
  t)

(def false (&rest args)
  "Return false for anything."
  (declare (ignore args))
  nil)

(defm dbg (&rest args)
  "Print information about ARGS, then return the result of evaluating ARGS."
  `(progn
     ,@(loop :for arg :in args
             :collect (if (stringp arg)
                          `(format t "~&~A~%" ,arg)
                          `(format t "~&~S: ~S~%" ',arg ,arg)))
     ,@args))

(defm dbg* ((&rest args) &body body)
  "Print information about ARGS, evaluate BODY, then return the result of evaluating ARGS."
  `(progn
     (dbg ,@args)
     ,@body
     ,@args))

(defm when-let (bindings &body forms)
  "Use BINDINGS like with LET, then evaluate FORMS if all BINDINGS evaluate to a
true value. This is ALEXANDRIA:WHEN-LET."
  (let* ((binding-list (if (and (consp bindings) (symbolp (car bindings)))
                           (list bindings)
                           bindings))
         (variables (mapcar #'car binding-list)))
    `(let ,binding-list
       (when (and ,@variables)
         ,@forms))))

(defm when-let* (bindings &body body)
  "Use BINDINGS like with LET*, then evaluate FORMS if all BINDINGS evaluate to
a true value. This is ALEXANDRIA:WHEN-LET*."
  (let ((binding-list (if (and (consp bindings) (symbolp (car bindings)))
                          (list bindings)
                          bindings)))
    (labels ((bind (bindings body)
               (if bindings
                   `(let (,(car bindings))
                      (when ,(caar bindings)
                        ,(bind (cdr bindings) body)))
                   `(progn ,@body))))
      (bind binding-list body))))

(def hyphenate (&rest names)
  "Return a new symbol from the hyphen concatenation of NAMES, then intern it in
the current package."
  (format nil "~{~A~^-~}"
          (mapcar (λ (name)
                    (string-upcase (marie/strings:string* name)))
                  names)))

(def hyphenate-intern (package &rest names)
  "Intern names from NAMES in PACKAGE with HYPHENATE."
  (let ((pkg (if (null package) *package* package)))
    (intern (apply #'hyphenate names) (find-package pkg))))

(def dump-table (table)
  "Print the contents of hash table TABLE."
  (maphash (λ (k v)
             (format t "~S => ~S~%" k v)
             (force-output *standard-output*))
           table))

(def dump-table* (table &optional (pad 0))
  "Print the contents of hash table TABLE recursively."
  (loop :for key :being :the :hash-keys :in table
        :for value :being :the :hash-values :in table
        :do (if (hash-table-p value)
                (progn
                  (format t "~A~S => ~S~%"
                          (make-string pad :initial-element #\space)
                          key
                          value)
                  (dump-table* value (+ pad 2)))
                (format t "~A~S => ~S~%"
                        (make-string pad :initial-element #\space)
                        key
                        value))))

(def muffle-debugger-handler (condition hook)
  "Define a handler for muffling the debugger messages."
  (declare (ignore hook))
  (format *error-output* "Caught error: ~A" condition)
  (finish-output *error-output*))

(def muffle-debugger ()
  "Hide debugger messages."
  (setf *debugger-hook* #'muffle-debugger-handler))

(defm with-muffled-debugger ((&optional) &body body)
  "Evaluate body with the debugger warnings turned off."
  `(let ((*debugger-hook* *debugger-hook*))
     (setf *debugger-hook* #'muffle-debugger-handler)
     ,@body))

(defm map-and (fn &rest args)
  "Return true if FN returns true for all items in ARGS."
  `(and ,@(loop :for arg :in args :collect `(funcall ,fn ,arg))
        t))

(defm map-or (fn &rest args)
  "Return true if FN returns true for at least one item in ARGS."
  `(or ,@(loop :for arg :in args :collect `(funcall ,fn ,arg))
       nil))

(defm rmap-and (value &rest fns)
  "Return true if all functions in FNS return true for VALUE."
  `(and ,@(loop :for fn :in fns :collect `(funcall ,fn ,value))
        t))

(defm rmap-or (value &rest fns)
  "Return true if at least one function in FNS return true for VALUE."
  `(or ,@(loop :for fn :in fns :collect `(funcall ,fn ,value))
       nil))

(defm (logical-and ∧) (&body body)
  "Return true if all forms in BODY evaluates to true."
  `(when (and ,@body)
     t))

(defm (logical-or ∨) (&body body)
  "Return true if all forms in BODY evaluates to false."
  `(when (or ,@body)
     t))

(defm ¬ (arg)
  "Return the negation of ARG."
  `(not ,arg))

(defm empty (object)
  "Set the value of OBJECT to null."
  `(setf ,object nil))

(defm empty* (&rest objects)
  "Set the value of OBJECTS to null."
  `(progn
     ,@(loop :for object :in objects
             :collect `(empty ,object))))

#+unix
(def getuid ()
  "Return the real UID of the user."
  #+sbcl (sb-posix:getuid)
  #+cmu (unix:unix-getuid)
  #+clisp (posix:uid)
  #+ecl (ext:getuid)
  #+ccl (ccl::getuid)
  #+allegro (excl.osi:getuid)
  #-(or sbcl cmu clisp ecl ccl allegro) (error "no getuid"))

(def gethash* (path table)
  "Return the value specified by path starting from TABLE."
  (cond ((marie/sequences:singlep path) (gethash (car path) table))
        ((null (hash-table-p (gethash (car path) table))) nil)
        (t (gethash* (cdr path)
                     (gethash (car path) table)))))

(def null* (value)
  "Return true if VALUE is null or every item is."
  (or (null value)
      (every #'null value)))

(defm eval-always (&body body)
  "Always evaluate BODY."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@body))
