;;;; -*- mode: lisp; syntax: common-lisp; base: 10; -*-
;;;; system.lisp --- ASDF helpers

(uiop:define-package #:marie/src/system
  (:use #:cl
        #:marie/src/definitions
        #:marie/src/etc))

(in-package #:marie/src/system)


;;; helpers

(def system-object^sys-object (name)
  "Return the system object for the current system."
  (asdf:find-system name))

(def system-path^sys-path (system)
  "Return the ASDF file path for the current system."
  (let ((object (system-object system)))
    (uiop:merge-pathnames* (cat system ".asd")
                           (asdf:system-source-directory object))))

(def read-system-path^read-sys-path (system)
  "Return the system ASDF file as s-expressions."
  (uiop:read-file-forms (sys-path system)))

(def system-version^sys-version (name)
  "Return the version number extracted from the system resources."
  (asdf:system-version (sys-object name)))

(def driver-path (system &optional (name "driver"))
  "Return the driver file of SYSTEM."
  (let* ((directory (asdf:system-source-directory system))
         (driver (make-pathname :directory '(:relative "src") :name name)))
    (uiop:merge-pathnames* driver directory)))

(def reload-driver (system)
  "Reload the driver file of SYSTEM."
  (let* ((path (driver-path system)))
    (load path)))

(defm reload-system (system)
  "Reload SYSTEM using symbol name."
  `(let* ((base (prin1-to-string ',system))
          (string (string-downcase base))
          (*standard-output* (make-broadcast-stream)))
     (mute
       (reload-driver string)
       (values))))
