;;;; misc.lisp

;;; Miscellaneous utilities

(in-package :mof)

;;; From http://thread.gmane.org/gmane.lisp.steel-bank.general/1598/focus=1604
(defmacro with-echo-off (&body body)
  "Disable terminal input echo within BODY"
  (with-gensyms (res)
    `(let ((,res nil))
       #+SBCL
       (let ((tm (sb-posix:tcgetattr sb-sys:*tty*)))
         (setf (sb-posix:termios-lflag tm)
               (logandc2 (sb-posix:termios-lflag tm) sb-posix:echo))
         (sb-posix:tcsetattr sb-sys:*tty* sb-posix:tcsanow tm))
       (setf ,res ,@body)
       #+SBCL
       (let ((tm (sb-posix:tcgetattr sb-sys:*tty*)))
         (setf (sb-posix:termios-lflag tm)
               (logior (sb-posix:termios-lflag tm) sb-posix:echo))
         (sb-posix:tcsetattr sb-sys:*tty* sb-posix:tcsanow tm))
       ,res)))

(defun read-passwd ()
  "Read a password string from standard input but do not echo the
characters being typed. Returns the input"
  (with-echo-off (read-line)))

(defun aps (symbol &optional (package *package*))
  "Shortcut for APROPOS."
  (loop
     :for i :in (sort (apropos-list symbol package) #'string<)
     :do (format t "~(~S~)~%" i)))

(defun doc (&rest args)
  "Shortcut for DOCUMENTATION."
  (apply #'documentation args))

(defmacro run (cmd &rest args)
  "Run command CMD and returns output as string."
  (with-gensyms (s)
    `(with-output-to-string (,s)
       #+SBCL
       (sb-ext:run-program ,cmd ',args :search t :output ,s)
       #+CCL
       (ccl:run-program ,cmd ',args :output ,s)
       (with-open-stream (in (#+(or CLISP CMUCL)
                                ext:run-program
                                ,cmd :arguments ,args :output :stream))
         (with-output-to-string (out)
           (loop
              :for line = (read-line in nil nil)
              :while line
              :do (write-line line out)))))))

(defmacro with-html (filename &body body)
  "Trivial wrapper for CL-WHO:WITH-HTML-OUTPUT."
  `(with-open-file (out ,filename
                        :direction :output
                        :if-exists :supersede)
     (cl-who:with-html-output (out nil :prologue t)
       ,@body)))

(defun read-integer (string)
  "Return integer from STRING"
  (parse-integer string :junk-allowed t))

(defun read-integer-line (file)
  "Return integer from a line in FILE"
  (read-integer (read-line file nil)))

(defun display-file (file)
  "Display the contents of FILE"
  (let ((in (open file :if-does-not-exist nil)))
  (when in
    (loop :for line = (read-line in nil)
       :while line
       :do (format t "~A~%" line))
    (close in))))

(defun collect-characters (start end)
  "Collect ASCII characters from START to END"
  (loop :for index :from start :below (+ start end) :collect (code-char index)))

