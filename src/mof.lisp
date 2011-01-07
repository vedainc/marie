;; Date created: <2007-10-02 16:52 rmm>

(in-package :mof)

(defmacro defalias (alias name)
  "Create alias `alias' for function `name'."
  `(defun ,alias (&rest args)
     (apply #',name args)))

;; From A Gentle Introduction to Symbolic Computation -
;; David Touretzky
(defmacro ppmx (form)
  "Pretty prints the macro expansion of FORM."
  `(let* ((exp1 (macroexpand-1 ',form))
          (exp (macroexpand exp1))
          (*print-circle* nil))
     (cond ((equal exp exp1)
            (format t "~&Macro expansion:")
            (pprint exp))
           (t (format t "~&First step of expansion:")
              (pprint exp1)
              (format t "~%~%Final expansion:")
              (pprint exp)))
     (format t "~%~%")
     (values)))

;; From Practical Common Lisp - Peter Seibel
(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop :for n :in names :collect `(,n (gensym)))
     ,@body))

(defmacro with-html (filename &body body)
  "Trivial wrapper for CL-WHO:WITH-HTML-OUTPUT."
  `(with-open-file (out ,filename
			:direction :output
			:if-exists :supersede)
     (cl-who:with-html-output (out nil :prologue t)
       ,@body)))

(defmacro write-html (&key output (title "") (styles '("style.css")) body)
  `(with-html
       ,output
     (:html
      :xmlns "http://www.w3.org/1999/xhtml"
      :xml\:lang "en"
      :xmlns\:xsi "http://www.w3.org/2001/XMLSchema-instance"
      (:head
       (:meta :http-equiv "Content-Type" :content "application/xhtml+xml")
       ,@(loop :for s :in styles :collect
            `(:link :href ,s :rel "stylesheet" :type "text/css"))
       (:title ,title))
      (:body
       ,@body))))

(defmacro with-echo-off (&body body)
  "Disable terminal input echo within BODY.
See http://thread.gmane.org/gmane.lisp.steel-bank.general/1598/focus=1604"
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
characters being typed. Returns the input."
  (with-echo-off (read-line)))

(defun bracket-reader (stream char)
  "Use [+ _ 1] as a shorthand for #'(lambda (_) (+ _ 1))
See http://www.bradediger.com/blog/2008/03/stealing_from_arc.html"
  (declare (ignore char))
  `(lambda (,(intern "_") &optional ,(intern "__"))
     (declare (ignorable ,(intern "__")))
     ,(read-delimited-list #\] stream t)))

(set-macro-character #\[ #'bracket-reader)
(set-macro-character #\] (get-macro-character #\) nil))

(defun brace-reader (stream char)
  "Use {foo 5} as a shorthand for (funcall foo 5)
See http://dorophone.blogspot.com/2008/03/common-lisp-reader-macros-simple.html"
  (declare (ignore char))
  `(funcall ,@(read-delimited-list #\} stream t)))

(set-macro-character #\{ #'brace-reader)
(set-macro-character #\} (get-macro-character #\) nil))

;; Use #L as a shorthand notation for #'(lambda (_) ...)
;; From: http://rottcodd.wordpress.com/2007/11/29/lambda-shortcut/
(set-dispatch-macro-character
 #\# #\L
 #'(lambda (stream sub-character infix-parameter)
     (when infix-parameter
       (error "#~a does not take an integer infix parameter."
	      sub-character))
     `#'(lambda (,(intern "_"))
	  ,(read stream t nil t))))

(defgeneric last1 (x)
  (:documentation "Return last element of sequence X"))

(defmethod last1 ((x list))
  (car (last x)))

(defmethod last1 ((x string))
  (let ((l (length x)))
    (aref x (- l 1))))

;; Return first expression from the last expression evaluated.
(set-dispatch-macro-character
 #\# #\^
 #'(lambda (stream sub-character infix-parameter)
     (when infix-parameter
       (error "#~a does not take an integer infix parameter."
	      sub-character))
     `(car /)))

;; Return last expression from the last expression evaluated.
(set-dispatch-macro-character
 #\# #\$
 #'(lambda (stream sub-character infix-parameter)
     (when infix-parameter
       (error "#~a does not take an integer infix parameter."
	      sub-character))
     `(last1 /)))
#+NIL
(defmacro install (&rest names)
  "Installs a package via `asdf-install' but automatically selects `skip-gpg-error'
restart when available."
  `(handler-bind ,(append
		   (loop :for condition :in '(asdf-install::gpg-error
                                              asdf-install::download-error
                                              asdf-install::key-not-found)
		      collect `(,condition
				#'(lambda (c)
				    (declare (ignore c))
				    (let ((restart (find-restart 'asdf-install::skip-gpg-check)))
				      (when restart
					(format t "~&** Invoking \"~S\" restart...~%" restart)
					(invoke-restart restart))))))
		   (list
;;; 		    '(asdf-install::download-error
;;; 		      #'(lambda (c)
;;; 			  (declare (ignore c))
;;; 			  (let ((restart (find-restart 'abort)))
;;; 			    (when restart
;;; 			      (invoke-restart restart)))))
		    ))
     (asdf-install:install ,@names)))

;; From Practical Common Lisp - Peter Seibel
(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop :for n :in names :collect `(,n (gensym)))
     ,@body))

(defun ap (symbol &optional package)
  "Shortcut for APROPOS."
  (loop :for i :in (sort (apropos-list symbol package) #'string<)
     :do (format t "~(~S~)~%" i)))

(defun unrequire (system)
  "Unloads a system, deleting the package and doing some cleanups."
  (let ((package (find-package system))
	(system (string-downcase (string system))))
    (when package
      (handler-bind
	  ((error
	    #'(lambda (c)
		(declare (ignore c))
		(let ((restart (find-restart 'continue)))
		  (when restart
		    (format t "~&** Invoking \"~S\" restart...~%" restart)
		    (invoke-restart restart))))))
	(delete-package package)
	(remhash system asdf::*defined-systems*)))))

(defmacro print-symbols (package &key (location :external-symbols))
  "Print symbols in a package. Prints external symbols by default."
  (let ((pkg (find-package package)))
    `(loop :for symbol being the ,location :in ,pkg
	:do (format t "~S~%" symbol))))

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

(defun digest (str &optional (type :sha256))
  "Return a string of a digest of a string."
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    type
    (#+SBCL
     sb-ext:string-to-octets
     #+CCL
     ccl:encode-string-to-octets
     #+CLISP
     ext:convert-string-to-bytes
     #+CMUCL
     stream:string-to-octets
     str
     #+CLISP
     charset:utf-8))))

(defun asdf (lib)
  "Shorthand to ASDF:LOAD-OP"
  (asdf:oos 'asdf:load-op lib))

(defun string-if (x)
  "Like IF but returns an empty string when X is false."
  (if x x ""))

