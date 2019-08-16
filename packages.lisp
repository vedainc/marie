;;;; packages.lisp

;;; Top-level package definition

(in-package #:cl-user)

(defpackage #:mof
  (:use #:cl)
  (:nicknames #:m)
  (:export
   ;; sequences
   #:last*
   #:solop
   #:longerp
   #:partition
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
   #:map-append
   #:map-nappend

   ;; strings
   #:empty-string-p
   #:digest-string
   #:string-if
   #:cat
   #:string-list
   #:split-string
   #:join-strings
   #:normalize-strings
   #:trim-whitespace
   #:fmt

   ;; symbols
   #:defcon
   #:defalias
   #:defun*
   #:with-gensyms
   #:ppmx

   ;; misc
   #+sbcl #:with-echo-off
   #+sbcl #:read-passwd
   #:symbols
   #:aps
   #:doc
   #:run
   #:with-html
   #:read-integer
   #:read-integer-line
   #:display-file
   #:collect-characters
   #:copy-hash-table
   #:home
   #:expand-pathname
   #:make
   #:make!
   #:with-time
   #:with-profiling
   #:true
   #:false

   ;; collect
   #:collect

   ;; files
   #:files

   ;; grids
   #:index-string
   #:map-string
   #:build-lines
   #:build-grid
   #:coordinates
   #:element
   #:elements
   #:print-grid
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
   #:grid-rows
   #:grid-columns
   #:group-coordinates
   #:group-elements
   #:range
   #:sort-coordinates))
