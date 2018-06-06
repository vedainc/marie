;;;; packages.lisp

;;; Top-level package definition

(in-package :cl-user)

(defpackage :mof
  (:use #:cl)
  (:nicknames #:m)
  (:export
   ;; sequences.lisp
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
   #:append*
   #:vector-list
   #:list-vector
   #:remove-items
   #:join-stream-string
   #:group-alike
   #:build-length-index

   ;; strings.lisp
   #:digest-string
   #:string-if
   #:cat
   #:string-list
   #:split-string
   #:join-strings
   #:normalize-strings
   #:trim-whitespace

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
   #:collect-characters
   #:copy-hash-table

   ;; collect.lisp
   #:collect

   ;; files.lisp
   #:files

   ;; matrix.lisp
   #:index-string
   #:map-string
   #:build-lines
   #:build-matrix
   #:coordinates
   #:element
   #:elements
   #:print-matrix
   #:dimensions
   #:coordinate-equal-p
   #:valid-coordinate-p
   #:adjacent-coordinates
   #:cross-adjacent-coordinates
   #:ensure-coordinate
   #:peek-up
   #:peek-right
   #:peek-down
   #:peek-left
   #:find-element-coordinates
   #:filter-lines
   #:line-values
   #:list-string
   #:line-string
   #:last-coordinate
   #:matrix-rows
   #:matrix-columns
   #:group-coordinates
   #:group-elements
   #:range
   #:sort-coordinates))
