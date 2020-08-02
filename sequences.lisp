;;;; sequences.lisp - utilities for working with lists and trees

(uiop:define-package #:marie/sequences
  (:use #:cl)
  (:export #:last*
           #:length=
           #:length<
           #:length>
           #:length<=
           #:length>=
           #:single
           #:singlep
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
           #:reduce-append
           #:join
           #:assoc-key
           #:assoc-value
           #:mem
           #:mem*
           #:remove*
           #:sequence-string
           #:butrest))

(in-package #:marie/sequences)

(defun last* (list)
  "Return the first of the last element of LIST."
  (first (last list)))

(defun length= (seq len)
  "Return true if the length of SEQ is LEN."
  (declare (type sequence seq))
  (= (length seq) len))

(defun length< (seq len)
  "Return true if the length of SEQ is LEN."
  (declare (type sequence seq))
  (< (length seq) len))

(defun length> (seq len)
  "Return true if the length of SEQ is LEN."
  (declare (type sequence seq))
  (> (length seq) len))

(defun length<= (seq len)
  "Return true if the length of SEQ is LEN."
  (declare (type sequence seq))
  (<= (length seq) len))

(defun length>= (seq len)
  "Return true if the length of SEQ is LEN."
  (declare (type sequence seq))
  (>= (length seq) len))

(defun single (seq)
  "Return the only item in SEQUENCE if SEQUENCE has only one element."
  (if (null (length= seq 1))
      (error "Argument must exactly be of length 1.")
      (elt seq 0)))

(defun singlep (list)
  "Return true if there is only one element in LIST."
  (and (consp list)
       (null (cdr list))))

(defun longerp (x y)
  "Return true if X is longer than Y."
  (labels ((fn (x y)
             (and (consp x)
                  (or (null y)
                      (fn (cdr x) (cdr y))))))
    (if (and (listp x) (listp y))
        (fn x y)
        (> (length x) (length y)))))

(defun partition (source n)
  "Create partition of N from SOURCE."
  (when (zerop n) (error "Zero length"))
  (labels ((fn (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (fn rest (cons (subseq source 0 n) acc))
                   (nreverse (cons source acc))))))
    (when source
      (fn source nil))))

(defun flatten-list (list)
  "Merge all symbols from LIST to one list."
  (labels ((fn (list acc)
             (cond ((null list) acc)
                   ((atom list) (cons list acc))
                   (t (fn (car list) (fn (cdr list) acc))))))
    (fn list nil)))

(defun filter-if (fn list)
  "Collect the results of applying FN to  LIST which returns true."
  (let ((acc nil))
    (dolist (x list)
      (let ((value (funcall fn x)))
        (when value (push value acc))))
    (nreverse acc)))

(defun filter-if-not (fn list)
  "Collect the results of applying FN to LIST which returns false."
  (filter-if (complement fn) list))

(defun prune-if (fn tree)
  "Remove all items from TREE to which FN returns true."
  (labels ((fn (tree acc)
             (cond ((null tree) (nreverse acc))
                   ((consp (car tree)) (fn (cdr tree)
                                           (cons (fn (car tree) nil)
                                                 acc)))
                   (t (fn (cdr tree)
                          (if (funcall fn (car tree))
                              acc
                              (cons (car tree) acc)))))))
    (fn tree nil)))

(defun prune-if-not (fn tree)
  "Remove all items from TREE to which FN returns false."
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
  "Return true if X occurs before Y in LIST."
  (when list
    (let ((first (car list)))
      (cond ((funcall test y first) nil)
            ((funcall test x first) list)
            (t (beforep x y (cdr list) :test test))))))

(defun afterp (x y list &key (test #'eql))
  "Return true if X occurs after Y in LIST."
  (let ((rest (beforep y x list :test test)))
    (when rest
      (member x rest :test test))))

(defun duplicatep (x list &key (test #'eql))
  "Return true if X has a duplicate in LIST."
  (member x (cdr (member x list :test test)) :test test))

(defun split-if (fn list)
  "Return two lists wherein the first list contains everything that satisfies FN, until it
doesn't, and another list that starts where FN returns true,as values."
  (let ((acc nil))
    (do ((source list (cdr source)))
        ((or (null source) (funcall fn (car source)))
         (values (nreverse acc) source))
      (push (car source) acc))))

(defun append* (list data)
  "Destructively update list with data."
  (setf list (nconc list data)))

(defun vector-list (list)
  "Return list as vector."
  (map 'vector #'identity list))

(defun list-vector (vector)
  "Return list as vector."
  (map 'list #'identity vector))

(defun remove-items (list items)
  "Remove ITEMS from LIST."
  (cond ((null items) list)
        (t (remove-items
            (remove (first items) list :test #'equal)
            (rest items)))))

(defun join-stream-string (stream lines)
  "Read lines from 1 to END from STREAM."
  (join (loop :for i :from 1 :to lines :collect (read-line stream nil nil))))

(defun group-alike (list)
  "Group similar elements together."
  (labels ((fn (list acc)
             (cond ((null list) (nreverse acc))
                   (t (fn (remove (first list) list)
                          (cons (make-list (count (first list) list) :initial-element (first list))
                                acc))))))
    (fn list nil)))

(defun build-length-index (groups)
  "Return a hash table from a list of lists, with the first member of each list as the key
and the length of each list as the value."
  (let ((table (make-hash-table :test #'equal)))
    (loop :for group :in groups
          :do (setf (gethash (first group) table) (length group)))
    table))

(defun map-append (fn sequence1 sequence2)
  "Apply APPEND to the result of applying FN to sequence1 and sequence2."
  (append (mapcar fn sequence1) (mapcar fn sequence2)))

(defun map-nappend (fn sequence1 sequence2)
  "Apply NCONC to the result of applying FN to sequence1 and sequence2."
  (nconc (mapcar fn sequence1) (mapcar fn sequence2)))

(defun reduce-append (&rest args)
  "Apply APPEND with REDUCE to ARGS."
  (if (length= args 1)
      (reduce #'append (car args))
      (reduce #'append args)))

(defun join (list &optional (char #\Space))
  "Merge items in LIST by the space character."
  (let ((fmt (marie/strings:cat "~{~A~^" (string char) "~}")))
    (format nil fmt list)))

(defun assoc-key (key items &key (test #'equal))
  "Return the key found in ITEMS if KEY is found."
  (let ((val (assoc key items :test test)))
    (when val
      (car val))))

(defun assoc-value (key items &key (test #'equal))
  "Return the value found in ITEMS if KEY is found."
  (let ((val (assoc key items :test test)))
    (when val
      (cdr val))))

(defun mem (elem list &key (test #'equal))
  "Return true if ELEM is a member of LIST using TEST as the equality function."
  (when (member elem list :test test)
    t))

(defun mem* (elems list &key (test #'equal))
  "Return true if all items ELEMS are members of LIST using TEST as the equality function."
  (labels ((fn (args)
             (cond ((null args) t)
                   ((member (car args) list :test test) (fn (cdr args)))
                   (t nil))))
    (or (funcall test elems list)
        (fn elems))))

(defun remove* (elems list &key (test #'equal))
  "Remove all items in ELEMS in LIST."
  (labels ((fn (args list)
             (cond ((null args) list)
                   (t (fn (cdr args) (remove (car args) list :test test))))))
    (fn elems list)))

(defun sequence-string (seq)
  "Return SEQ as a string."
  (format nil "~{~A~}" seq))

(defun butrest (list)
  "Return everything from LIST except the rest."
  (butlast list (1- (length list))))
