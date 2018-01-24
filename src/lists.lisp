;;;; lists.lisp

;;; Utilities for working with lists and trees

(in-package :mof)

(defun last* (list)
  "Return the first of the last element of LIST"
  (first (last list)))

(defun solop (list)
  "Return true if there is only one element in LIST"
  (and (consp list) (null (cdr list))))

(defun longerp (x y)
  "Return true if X is longer than Y"
  (labels ((fn (x y)
             (and (consp x)
                  (or (null y)
                      (fn (cdr x) (cdr y))))))
    (if (and (listp x) (listp y))
        (fn x y)
        (> (length x) (length y)))))

(defun group (source n)
  "Create groups of N from SOURCE"
  (when (zerop n) (error "Zero length"))
  (labels ((fn (source accumulator)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (fn rest (cons (subseq source 0 n) accumulator))
                   (nreverse (cons source accumulator))))))
    (when source
      (fn source nil))))

(defun flatten-list (list)
  "Merge all symbols from LIST to one list"
  (labels ((fn (list accumulator)
             (cond ((null list) accumulator)
                   ((atom list) (cons list accumulator))
                   (t (fn (car list) (fn (cdr list) accumulator))))))
    (fn list nil)))

(defun filter-if (fn list)
  "Collect the results of applying FN to  LIST which returns true"
  (let ((accumulator nil))
    (dolist (x list)
      (let ((value (funcall fn x)))
        (when value (push value accumulator))))
    (nreverse accumulator)))

(defun filter-if-not (fn list)
  "Collect the results of applying FN to LIST which returns false"
  (filter-if (complement fn) list))

(defun prune-if (fn tree)
  "Remove all items from TREE to which FN returns true"
  (labels ((fn (tree accumulator)
             (cond ((null tree) (nreverse accumulator))
                   ((consp (car tree)) (fn (cdr tree)
                                           (cons (fn (car tree) nil)
                                                 accumulator)))
                   (t (fn (cdr tree)
                          (if (funcall fn (car tree))
                              accumulator
                              (cons (car tree) accumulator)))))))
    (fn tree nil)))

(defun prune-if-not (fn tree)
  "Remove all items from TREE to which FN returns false"
  (prune-if (complement fn) tree))

(defun locate-if (fn list)
  "Find element in list satisfying FN. When found, return the car of LIST and the result of applying
FN, as values. Otherwise, return false."
  (unless (null list)
    (let ((val (funcall fn (car list))))
      (if val
          (values (car list) val)
          (find-if fn (cdr list))))))

(defun beforep (x y list &key (test #'eql))
  "Return true if X occurs before Y in LIST"
  (when list
    (let ((first (car list)))
      (cond ((funcall test y first) nil)
            ((funcall test x first) list)
            (t (beforep x y (cdr list) :test test))))))

(defun afterp (x y list &key (test #'eql))
  "Return true if X occurs after Y in LIST"
  (let ((rest (beforep y x list :test test)))
    (when rest
      (member x rest :test test))))

(defun duplicatep (x list &key (test #'eql))
  "Return true if X has a duplicate in LIST"
  (member x (cdr (member x list :test test)) :test test))

(defun split-if (fn list)
  "Return two lists wherein the first list contains everything that satisfies FN, until it
doesn't, and another list that starts where FN returns true,as values"
  (let ((accumulator nil))
    (do ((source list (cdr source)))
        ((or (null source) (funcall fn (car source)))
         (values (nreverse accumulator) source))
      (push (car source) accumulator))))


