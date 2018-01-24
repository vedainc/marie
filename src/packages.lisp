;;;; packages.lisp

;;; Top-level package definition

(in-package :cl-user)

(defpackage :mof
  (:use #:cl)
  (:nicknames #:m)
  (:export
   ;; lists.lisp
   #:last*
   #:solop
   #:longerp
   #:group
   #:flatten-list
   #:filter-if
   #:filter-if-not
   #:prune-if
   #:prune-if-not
   #:locate-if
   #:beforep
   #:afterp
   #:duplicatep
   #:split-if

   ;; strings.lisp
   #:digest-string
   #:string-if
   #:cat
   #:string-list
   #:split-string
   #:join-strings
   #:normalize-strings

   ;; symbols.lisp
   #:defalias
   #:with-gensyms
   #:ppmx

   ;; misc.lisp
   #:with-echo-off
   #:read-passwd
   #:print-symbols
   #:aps
   #:doc
   #:run
   #:with-html
   #:read-integer
   #:read-integer-line
   #:display-file
   #:make

   ;; matrix.lisp
   #:index-string
   #:map-string
   #:build-words
   #:build-matrix
   #:coordinates
   #:element
   #:elements
   #:print-matrix
   #:dimensions
   #:coordinate-equal-p
   #:valid-coordinate-p
   #:ensure-coordinate
   #:peek-up
   #:peek-right
   #:peek-down
   #:peek-left
   #:find-element-coordinates
   #:filter-lines
   #:line-values
   #:list-string
   #:line-string))
