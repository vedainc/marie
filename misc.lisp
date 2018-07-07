;;;; misc.lisp

;;; Miscellaneous utilities

(in-package #:mof)

;;; From http://thread.gmane.org/gmane.lisp.steel-bank.general/1598/focus=1604
#+sbcl
(defmacro with-echo-off (&body body)
  "Disable terminal input echo within BODY."
  (with-gensyms (res)
    `(let ((,res nil))
       #+sbcl
       (let ((tm (sb-posix:tcgetattr sb-sys:*tty*)))
         (setf (sb-posix:termios-lflag tm)
               (logandc2 (sb-posix:termios-lflag tm) sb-posix:echo))
         (sb-posix:tcsetattr sb-sys:*tty* sb-posix:tcsanow tm))
       (setf ,res ,@body)
       #+sbcl
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

(defun make (system &key (force nil))
  "Use ASDF to load systems."
  (asdf:make system :force force))

(defun make! (system)
  "Use ASDF to force build "
  (make system :force t))
