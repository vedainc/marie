;;;; etc.lisp
;;;; Utilities that don't fit elsewhere

(uiop:define-package #:marie/etc
    (:use #:cl
          #:marie/definitions
          #:marie/sequences
          #:marie/strings))

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

(def hyphenate-to-string (&rest names)
  "Return a new string from the hyphenated concatenation of NAMES."
  (format nil "~{~A~^-~}"
          (mapcar (λ (name)
                    (string-upcase (string* name)))
                  names)))

(def hyphenate-to-symbol (&rest names)
  "Apply HYPHENATE to NAMES then return it as a symbol."
  (read-from-string (apply #'hyphenate-to-string names)))

(def hyphenate-to-interned-symbol (package &rest names)
  "Apply HYPHENATE to NAMES then return an interned symbol in the current package."
  (let ((pkg (if (null package) *package* package)))
    (intern (apply #'hyphenate-to-string names) (find-package pkg))))

(def show-table (table)
  "Print the contents of hash table TABLE."
  (maphash (λ (k v)
             (format t "~S => ~S~%" k v)
             (force-output *standard-output*))
           table))

(def show-table* (table &optional (pad 0))
  "Print the contents of hash table TABLE recursively."
  (loop :for key :being :the :hash-keys :in table
        :for value :being :the :hash-values :in table
        :do (if (hash-table-p value)
                (progn
                  (format t "~A~S => ~S~%"
                          (make-string pad :initial-element #\space)
                          key
                          value)
                  (show-table* value (+ pad 2)))
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
  #+lispworks (sys::get-user-id)
  #+allegro (excl.osi:getuid)
  #-(or sbcl cmu clisp ecl ccl lispworks allegro) (error "no getuid"))

#-unix
(def getuid ()
  (error "no getuid"))

(def gethash* (path table)
  "Return the value specified by path starting from TABLE."
  (cond ((singlep path) (gethash (car path) table))
        ((null (hash-table-p (gethash (car path) table))) nil)
        (t (gethash* (cdr path)
                     (gethash (car path) table)))))

(def null* (value)
  "Return true if VALUE is null or every item is."
  (or (null value)
      (every #'null value)))

(defm eval-always (&body body)
  "Evaluate the forms in BODY in all situations."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@body))

(def fmt-error (string)
  "Output STRING to *STANDARD-ERROR* then return."
  (format *error-output* string)
  (finish-output *error-output*))

(defmm appendf (&rest lists) append
  "Set the value of the first argument to the result of applying APPEND to LISTS.")

(defmm maxf (&rest numbers) max
  "Set the value of the first argument to the result of applying MAX to NUMBERS.")

(defmm minf (&rest numbers) min
  "Set the value of the first argument to the result of applying MIN to NUMBERS.")
