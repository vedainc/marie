;;;; misc.lisp

;;; Miscellaneous utilities

(in-package #:mof)

;;; From http://thread.gmane.org/gmane.lisp.steel-bank.general/1598/focus=1604
#+(and sbcl unix)
(defmacro with-echo-off (&body body)
  "Disable terminal input echo within BODY."
  (with-gensyms (res)
    `(let ((,res nil))
       (let ((tm (sb-posix:tcgetattr sb-sys:*tty*)))
         (setf (sb-posix:termios-lflag tm)
               (logandc2 (sb-posix:termios-lflag tm) sb-posix:echo))
         (sb-posix:tcsetattr sb-sys:*tty* sb-posix:tcsanow tm))
       (setf ,res ,@body)
       (let ((tm (sb-posix:tcgetattr sb-sys:*tty*)))
         (setf (sb-posix:termios-lflag tm)
               (logior (sb-posix:termios-lflag tm) sb-posix:echo))
         (sb-posix:tcsetattr sb-sys:*tty* sb-posix:tcsanow tm))
       ,res)))

#+sbcl
(defun read-passwd ()
  "Read a password string from standard input but do not echo the
characters being typed. Returns the input."
  (with-echo-off (read-line)))

(defun aps (symbol &optional (package *package*))
  "Shortcut for APROPOS."
  (loop :for i :in (sort (apropos-list symbol package) #'string<)
        :do (format t "~(~S~)~%" i)))

(defun doc (&rest args)
  "Shortcut for DOCUMENTATION."
  (apply #'documentation args))

(defmacro run (cmd &rest args)
  "Run command CMD and returns output as string."
  (with-gensyms (s)
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

(defmacro with-profiling (&body body)
  "Run the profiler with BODY."
  #+sbcl
  `(sb-sprof:with-profiling (:report :graph :show-progress t) ,@body))

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

