;;;; -*- mode: lisp; syntax: common-lisp; base: 10; coding: utf-8-unix; external-format: (:utf-8 :eol-style :lf); -*-
;;;; etc.lisp ---  utilities that don’t fit elsewhere

(uiop:define-package #:marie/src/etc
  (:use #:cl
        #:marie/src/definitions
        #:marie/src/sequences
        #:marie/src/strings))

(in-package #:marie/src/etc)


;;; Optimiziation

(defm optimize-safety ()
  "Enable compiler options for maximum safety options."
  `(declaim (optimize (safety 3) (debug 3) (speed 0))))

(defm optimize-speed ()
  "Enable compiler options for maximum speed options."
  `(declaim (optimize (safety 1) (debug 3) (speed 3) )))

(defm optimize-speed-unsafe ()
  "Enable compiler options for maximum speed options."
  `(declaim (optimize (safety 0) (debug 3) (speed 3))))

(defm optimize-debug ()
  "Enable compiler options for maximum debug settings."
  `(declaim (optimize (speed 1) (debug 3) (safety 1))))



;;; Debugging

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

(defm dbg1 (arg &body body)
  "Apply DBG to ARG, then evaluate ARGS."
  `(progn
     (dbg ,arg)
     ,@body))

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


;;; Hyphenated fns

(def hyphenate-to-string (&rest names)
  "Return a new string from the hyphenated concatenation of NAMES."
  (format nil "~{~A~^-~}"
          (mapcar (lambda (name)
                    (string-upcase (string* name)))
                  names)))

(def hyphenate-to-symbol (&rest names)
  "Apply HYPHENATE to NAMES then return it as a symbol."
  (read-from-string (apply #'hyphenate-to-string names)))

(def hyphenate-to-interned-symbol (package &rest names)
  "Apply HYPHENATE to NAMES then return an interned symbol in the current package."
  (let ((pkg (if (null package) *package* package)))
    (intern (apply #'hyphenate-to-string names) (find-package pkg))))

;;; (empty - NULL)

(defm empty (object)
  "Set the value of OBJECT to null."
  `(setf ,object nil))

(defm empty* (&rest objects)
  "Set the value of OBJECTS to null."
  `(progn
     ,@(loop :for object :in objects
             :collect `(empty ,object))))

(def null* (value)
  "Return true if VALUE is null or every item is."
  (or (null value)
      (every #'null value)))


;;;  Miscellaneous fns

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


(defm eval-always (&body body)
  "Evaluate the forms in BODY in all situations."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@body))

(defmm appendf (&rest lists) append
  "Set the value of the first argument to the result of applying APPEND to LISTS.")

(defmm maxf (&rest numbers) max
  "Set the value of the first argument to the result of applying MAX to NUMBERS.")

(defmm minf (&rest numbers) min
  "Set the value of the first argument to the result of applying MIN to NUMBERS.")

(defm defselectors (prefix count)
  "Define list selectors prefixed with PREFIX that will act as sequence accessors."
  `(progn
     ,@(loop :for n :from 0 :to count
             :for name = (read-from-string (concat prefix (write-to-string n)))
             :collect `(def ,name (list) (elt list ,n)))))

(defm fns (fn1 &rest args)
  "Return a function that applies FN1 and ARGS to OBJ that returns multiple values."
  `(lambda (obj)
     (list (funcall ,fn1 obj)
           ,@(loop :for arg :in args :collect `(funcall ,arg obj)))))

(defm with-suppresed-output^mute (&body body)
  "Evaluate BODY but with output suppressed."
  `(let ((*standard-output* (make-broadcast-stream))
         (*debug-io* (make-broadcast-stream))
         (*error-output* (make-broadcast-stream)))
     ,@body))

(defm mulf (var num)
  "Set a new value for VAR by multiplying itself by a number."
  `(setf ,var (* ,var ,num)))

(def prn (object)
  "Print OBJECT according to its type."
  (cond
    ((typep object 'hash-table) (show-table* object))
    ((typep object 'vector) (loop :for item :across object :do (format t "~S~%" item)))
    ((typep object 'cons) (format t "~{~S~%~}" object))
    (t (format t "~A~%" object))))

(def apropos* (&rest args)
  "Display sorted matching symbols from ARGS with CL:APROPOS."
  (loop :for symbol :in (sort (apply #'apropos-list args) #'string<)
        :do (format t "~S~%" symbol)))

(def collect-characters (start end)
  "Collect ASCII characters from START to END."
  (loop :for index :from start :below (+ start end) :collect (code-char index)))
