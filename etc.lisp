;;;; etc.lisp - miscellaneous utilities

(uiop:define-package #:marie/etc
  (:use #:cl)
  (:export #:symbols
           #:aps
           #:doc
           #:run
           #:with-html
           #:read-integer
           #:read-integer-line
           #:display-file
           #:collect-characters
           #:copy-hash-table
           #:home
           #:expand-pathname
           #:make
           #:make!
           #:with-time
           #:true
           #:false
           #:dbg
           #:dbg*
           #:f-and
           #:f-or
           #:when-let
           #:when-let*
           #:hyphenate
           #:hyphenate-intern
           #:dump-table
           #:dump-table*
           #:muffle-debugger
           #:with-muffled-debugger))

(in-package #:marie/etc)

(defun aps (symbol &optional (package *package*))
  "Shortcut for APROPOS."
  (loop :for i :in (sort (apropos-list symbol package) #'string<)
        :do (format t "~(~S~)~%" i)))

(defun doc (&rest args)
  "Shortcut for DOCUMENTATION."
  (apply #'documentation args))

(defmacro run (cmd &rest args)
  "Run command CMD and returns output as string."
  (marie/symbols:with-gensyms (s)
    `(with-output-to-string (,s)
       #+sbcl
       (sb-ext:run-program ,cmd ',args :search t :output ,s)
       #+ccl
       (ccl:run-program ,cmd ',args :output ,s)
       (with-open-stream (in (#+(or clisp cmucl)
                              ext:run-program
                              ,cmd :arguments ,args :output :stream))
         (with-output-to-string (out)
           (loop :for line = (read-line in nil nil)
                 :while line
                 :do (write-line line out)))))))

(defun read-integer (string)
  "Return integer from STRING."
  (parse-integer string :junk-allowed t))

(defun read-integer-line (file)
  "Return integer from a line in FILE."
  (read-integer (read-line file nil)))

(defun display-file (file)
  "Display the contents of FILE."
  (let ((in (open file :if-does-not-exist nil)))
    (when in
      (loop :for line = (read-line in nil)
            :while line
            :do (format t "~A~%" line))
      (close in))))

(defun collect-characters (start end)
  "Collect ASCII characters from START to END."
  (loop :for index :from start :below (+ start end) :collect (code-char index)))

(defun copy-hash-table (hash-table)
  "Create a new hash table from HASH-TABLE."
  (let ((table (make-hash-table :test (hash-table-test hash-table)
                                :rehash-size (hash-table-rehash-size hash-table)
                                :rehash-threshold (hash-table-rehash-threshold hash-table)
                                :size (hash-table-size hash-table))))
    (loop :for key :being each hash-key :of hash-table
          :using (hash-value value)
          :do (setf (gethash key table) value)
          :finally (return table))))

(defun home (path)
  "Return a path relative to the home directory."
  (uiop:subpathname (user-homedir-pathname) path))

(defun expand-pathname (path)
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

(defun make (system &key (force nil))
  "Use ASDF to load systems."
  (asdf:make system :force force))

(defun make! (system)
  "Use ASDF to force build "
  (make system :force t))

(defmacro with-time (&body body)
  "Execute BODY then return timing information."
  `(time (progn ,@body (values))))

(defun true (arg)
  "Return true for anything."
  (declare (ignore arg))
  t)

(defun false (arg)
  "Return false for anything."
  (declare (ignore arg))
  nil)

(defmacro dbg (&rest args)
  "Print information about ARGS."
  `(progn
     ,@(loop :for arg :in args
             :collect (if (stringp arg)
                          `(format t "~&~A~%" ,arg)
                          `(format t "~&~S: ~S~%" ',arg ,arg)))))

(defmacro dbg* ((&rest args) &body body)
  "Print information about ARGS, then evaluate BODY."
  `(progn (dbg ,@args)
          ,@body))

(defmacro f-and (v &rest fs)
  "Return the conjunction of FS on V."
  `(and ,@(loop :for f :in fs :collect `(funcall ,f ,v))))

(defmacro f-or (v &rest fs)
  "Return the disjunction of FS on V."
  `(or ,@(loop :for f :in fs :collect `(funcall ,f ,v))))

(defmacro when-let (bindings &body forms)
  "Use BINDINGS like with LET, then evaluate FORMS if all BINDINGS evaluate to a true value. This is ALEXANDRIA:WHEN-LET."
  (let* ((binding-list (if (and (consp bindings) (symbolp (car bindings)))
                           (list bindings)
                           bindings))
         (variables (mapcar #'car binding-list)))
    `(let ,binding-list
       (when (and ,@variables)
         ,@forms))))

(defmacro when-let* (bindings &body body)
  "Use BINDINGS like with LET*, then evaluate FORMS if all BINDINGS evaluate to a true value. This is ALEXANDRIA:WHEN-LET*."
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

(defun hyphenate (&rest names)
  "Return a new symbol from the hyphen concatenation of NAMES, then intern it in the current package."
  (format nil "~{~A~^-~}"
          (mapcar #'(lambda (name)
                      (string-upcase (marie/strings:string-convert name)))
                  names)))

(defun hyphenate-intern (package &rest names)
  "Intern names from NAMES in PACKAGE with HYPHENATE."
  (let ((p (if (null package) *package* package)))
    (intern (apply #'hyphenate names) (find-package p))))

(defun dump-table (table)
  "Print the contents of hash table TABLE."
  (maphash #'(lambda (k v)
               (format t "~S => ~S~%" k v))
           table))

(defun dump-table* (table &optional (pad 0))
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
                (format t "~S => ~S~%" key value))))

(defun muffle-debugger ()
  "Hide the debugger output."
  (setf *debugger-hook*
        (lambda (condition hook)
          (declare (ignore hook))
          (format *error-output* "Caught error: ~A" condition)
          (finish-output *error-output*))))

(defmacro with-muffled-debugger (&body body)
  "Evaluate body with the debugger warnings turned off."
  `(let ((*debugger-hook* *debugger-hook*))
     (muffle-debugger)
     ,@body))