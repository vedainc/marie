;;;; user.lisp --- user sandbox

(uiop:define-package #:${project}/src/user
  (:nicknames #:${project}-user)
  (:use #:cl
        #:marie
        #:${project}/src/driver))

(in-package #:${project}-user)
