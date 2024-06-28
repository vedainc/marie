;;;; -*- mode: lisp; syntax: common-lisp; base: 10; coding: utf-8-unix; external-format: (:utf-8 :eol-style :lf); -*-
;;;; sequences.lisp --- utilities for dealing with sequences

(uiop:define-package #:marie/src/sequences
  (:use #:cl
        #:marie/src/definitions
        #:marie/src/conditionals))

(in-package #:marie/src/sequences)

(def end (seq)
  "Return the last element of SEQ."
  (etypecase seq
    (cons (first (last seq)))
    (string (elt seq (1- (length seq))))
    (null nil)))

(def length= (seq len)
  "Return true if the length of SEQ is LEN."
  (declare (type sequence seq))
  (= (length seq) len))

(def length< (seq len)
  "Return true if the length of SEQ is LEN."
  (declare (type sequence seq))
  (< (length seq) len))

(def length> (seq len)
  "Return true if the length of SEQ is LEN."
  (declare (type sequence seq))
  (> (length seq) len))

(def length<= (seq len)
  "Return true if the length of SEQ is LEN."
  (declare (type sequence seq))
  (<= (length seq) len))

(def length>= (seq len)
  "Return true if the length of SEQ is LEN."
  (declare (type sequence seq))
  (>= (length seq) len))

(def singlep (seq)
  "Return true if there is only one item in SEQ."
  (length= seq 1))

(def single (seq)
  "Return the only item in SEQ if SEQ has only one element."
  (if (singlep seq)
      (elt seq 0)
      (error "Argument must exactly be of length 1.")))

(def longerp (x y)
  "Return true if X is longer than Y."
  (labels ((fn (x y)
               (and (consp x)
                    (or (null y)
                        (fn (cdr x) (cdr y))))))
    (if (and (listp x) (listp y))
        (fn x y)
        (> (length x) (length y)))))

(def partition (source n)
  "Create partition of N from SOURCE."
  (when (zerop n) (error "Zero length"))
  (labels ((fn (source acc)
               (let ((rest (nthcdr n source)))
                 (if (consp rest)
                     (fn rest (cons (subseq source 0 n) acc))
                     (nreverse (cons source acc))))))
    (when source
      (fn source nil))))

(def flatten-list (list)
  "Merge all symbols from LIST to one list."
  (labels ((fn (list acc)
               (cond ((null list) acc)
                     ((atom list) (cons list acc))
                     (t (fn (car list) (fn (cdr list) acc))))))
    (fn list nil)))

(def filter-if (fn list)
  "Collect the results of applying FN to LIST which returns true."
  (let ((acc nil))
    (dolist (x list)
      (let ((value (funcall fn x)))
        (when value (push value acc))))
    (nreverse acc)))

(def filter-if-not (fn list)
  "Collect the results of applying FN to LIST which returns false."
  (filter-if (complement fn) list))

(def prune-if (fn tree)
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

(def prune-if-not (fn tree)
  "Remove all items from TREE to which FN returns false."
  (prune-if (complement fn) tree))

(def locate-if (fn list)
  "Find element in list satisfying FN. When found, return the car of LIST and the result of applying
FN, as values. Otherwise, return false."
  (unless (null list)
    (let ((val (funcall fn (car list))))
      (if val
          (values (car list) val)
          (find-if fn (cdr list))))))

(def beforep (x y list &key (test #'eql))
  "Return true if X occurs before Y in LIST."
  (when list
    (let ((first (car list)))
      (cond ((funcall test y first) nil)
            ((funcall test x first) list)
            (t (beforep x y (cdr list) :test test))))))

(def afterp (x y list &key (test #'eql))
  "Return true if X occurs after Y in LIST."
  (let ((rest (beforep y x list :test test)))
    (when rest
      (member x rest :test test))))

(def duplicatep (x list &key (test #'eql))
  "Return true if X has a duplicate in LIST."
  (member x (cdr (member x list :test test)) :test test))

(def split-if (fn list)
  "Return two lists wherein the first list contains everything that satisfies FN, until it
doesn't, and another list that starts where FN returns true,as values."
  (let ((acc nil))
    (do ((source list (cdr source)))
        ((or (null source) (funcall fn (car source)))
         (values (nreverse acc) source))
      (push (car source) acc))))

(def append* (list data)
  "Destructively update list with data."
  (setf list (nconc list data)))

(def vector-list (list)
  "Return list as vector."
  (map 'vector #'identity list))

(def list-vector (vector)
  "Return list as vector."
  (map 'list #'identity vector))

(def remove-items (list items)
  "Remove ITEMS from LIST."
  (cond ((null items) list)
        (t (remove-items
            (remove (first items) list :test #'equal)
            (rest items)))))

(def group-alike (list)
  "Group similar elements together."
  (labels ((fn (list acc)
               (cond ((null list) (nreverse acc))
                     (t (fn (remove (first list) list)
                            (cons (make-list (count (first list) list) :initial-element (first list))
                                  acc))))))
    (fn list nil)))

(def build-length-index (groups)
  "Return a hash table from a list of lists, with the first member of each list as the key
and the length of each list as the value."
  (let ((table (make-hash-table :test #'equal)))
    (loop :for group :in groups
          :do (setf (gethash (first group) table) (length group)))
    table))

(def map-append (fn sequence1 sequence2)
  "Apply APPEND to the result of applying FN to sequence1 and sequence2."
  (append (mapcar fn sequence1) (mapcar fn sequence2)))

(def map-nconc (fn sequence1 sequence2)
  "Apply NCONC to the result of applying FN to sequence1 and sequence2."
  (nconc (mapcar fn sequence1) (mapcar fn sequence2)))

(def reduce-append^red-append (&rest args)
  "Reduce ARGS with APPEND."
  (flet ((fn (arg)
             (reduce #'append arg)))
    (if (length= args 1)
        (fn (car args))
        (fn args))))

(def reduce-nconc^red-nconc (&rest args)
  "Reduce ARGS with NCONC."
  (flet ((fn (arg)
             (reduce #'nconc arg)))
    (if (length= args 1)
        (fn (car args))
        (fn args))))

(def join (list &optional (pad " "))
  "Merge items in LIST by the space character."
  (let* ((separator (if (null pad) "" pad))
         (fmt (uiop:strcat "~{~A~^" separator "~}")))
    (format nil fmt list)))

(def join-stream (stream end)
  "Read lines from 1 to END from STREAM."
  (join (loop :for i :from 1 :to end
              :collect (read-line stream nil nil))))

(def assoc-key (key items &key (test #'equal))
  "Return the key found in ITEMS if KEY is found."
  (let ((val (assoc key items :test test)))
    (when val
      (car val))))

(def assoc-value (key items &key (test #'equal))
  "Return the value found in ITEMS if KEY is found."
  (let ((val (assoc key items :test test)))
    (when val
      (cdr val))))

(def mem (elem list &key (test #'equal))
  "Return true if ELEM is a member of LIST using TEST as the equality function."
  (when (member elem list :test test)
    t))

(def mem* (elems list &key (test #'equal))
  "Return true if all items ELEMS are members of LIST using TEST as the equality function."
  (labels ((fn (args)
               (cond ((null args) t)
                     ((member (car args) list :test test) (fn (cdr args)))
                     (t nil))))
    (or (funcall test elems list)
        (fn elems))))

(def remove* (elems list &key (test #'equal))
  "Remove all items in ELEMS in LIST."
  (labels ((fn (args list)
               (cond ((null args) list)
                     (t (fn (cdr args) (remove (car args) list :test test))))))
    (fn elems list)))

(def sequence-string (seq)
  "Return SEQ as a string."
  (format nil "~{~A~}" seq))

(def butrest (list)
  "Return everything from LIST except the rest."
  (butlast list (1- (length list))))

(def insert-after (list index item)
  "Return a new list from LIST where ITEM is inserted after INDEX."
  (let ((copy (copy-list list)))
    (push item (cdr (nthcdr index copy)))
    copy))

(def insert-before (list index item)
  "Return a new list from LIST where ITEM is inserted after INDEX."
  (let ((copy (copy-list list)))
    (if (zerop index)
        (push item copy)
        (insert-after copy (1- index) item))))

(def append1 (list obj)
  "Apply APPEND to LIST and OBJ ensuring that OBJ is a list."
  (append list (uiop:ensure-list obj)))

(def nconc1 (list obj)
  "Apply NCONC to LIST and OBJ ensuring that OBJ is a list."
  (nconc list (uiop:ensure-list obj)))

(def include-if (&rest args)
  "Apply REMOVE-IF-NOT to ARGS."
  (apply #'remove-if-not args))

(def take (seq count)
  "Return COUNT amount of items from SEQ."
  (loop :for s :in seq
        :for n = 0 :then (1+ n)
        :while (< n count)
        :collect s))

(def take-if (fn seq count)
  "Return COUNT amount of items from SEQ that satisfy FN."
  (loop :for s :in seq
        :for val = (funcall fn s)
        :for n = 0 :then (if val (1+ n) n)
        :when (and val (> count 0) (< n (1+ count)))
          :collect s))

(def drop (seq count)
  "Return items from SEQ without the first COUNT items."
  (loop :for s :in seq
        :for n = 1 :then (1+ n)
        :when (>= count n)
          :collect s))

(def drop-if (fn seq count)
  "Return items from SEQ without the first COUNT items that satisfy FN."
  (loop :for s :in seq
        :for val = (funcall fn s)
        :for n = 0 :then (if val (1+ n) n)
        :unless (and val (> count 0) (< n (1+ count)))
          :collect s))

(def every-list-p (object)
  "Return true if OBJECT is a list and all members are lists."
  (land (listp object)
        (every #'listp object)))

(def remove-from-plist (plist &rest keys)
  "Returns a property-list with same keys and values as PLIST, except that keys in the list designated
by KEYS and values corresponding to them are removed.  The returned property-list may share
structure with the PLIST, but PLIST is not destructively modified. Keys are compared using EQ."
  (declare (optimize speed))
  (loop :for (key . rest) :on plist :by #'cddr
        :do (assert rest () "Expected a proper plist, got ~S" plist)
        :unless (member key keys :test #'eq)
          :collect key :and :collect (first rest)))

(defmm remove-from-plistf (&rest keys)
  remove-from-plist
  "Modify macro for REMOVE-FROM-PLIST.")

(def delete-from-plist (plist &rest keys)
  "Just like REMOVE-FROM-PLIST, but this version may destructively modify the provided PLIST."
  (declare (optimize speed))
  (loop :with head = plist
        :with tail = nil                ; a nil tail means an empty result so far
        :for (key . rest) :on plist :by #'cddr
        :do (assert rest () "Expected a proper plist, got ~S" plist)
            (if (member key keys :test #'eq)
                ;; skip over this pair
                (let ((next (cdr rest)))
                  (if tail
                      (setf (cdr tail) next)
                      (setf head next)))
                ;; keep this pair
                (setf tail rest))
        :finally (return head)))

(defmm delete-from-plistf (&rest keys)
  delete-from-plist
  "Modify macro for DELETE-FROM-PLIST.")

(def make-empty-list (object)
  "Return an empty list from OBJECT."
  (let ((length (length object)))
    (make-list length :initial-element nil)))

(def groups (list &optional count)
  "Return decreasing order of groups from LIST.

  (groups '(a b c))

would return

  ((a b c) (b c) (c))"
  (maplist #'identity list))

(def pairs (list)
  "Return pairs of lists from LIST.

  (pairs '(a b c))

would return

  ((a b) (b c))"
  (labels ((fn (list &optional acc)
               (cond ((neg list) (nreverse acc))
                     ((length= list 1) (nreverse acc))
                     (t (fn (cdr list)
                            (cons (list (first list)
                                        (second list))
                                  acc))))))
    (fn list)))

(def array-to-list (array)
  "Return a list from ARRAY."
  (let* ((dimensions (array-dimensions array))
         (depth (1- (length dimensions)))
         (indices (make-list (1+ depth) :initial-element 0)))
    (labels ((fn (n)
                 (loop :for j :below (nth n dimensions)
                       :do (setf (nth n indices) j)
                       :collect (if (= n depth)
                                    (apply #'aref array indices)
                                    (fn (1+ n))))))
      (fn 0))))

(def permutations^perms (list)
  "Return the permutations of LIST."
  (cond ((null list) nil)
        ((null (cdr list)) (list list))
        (t (loop :for element :in list
                 :append (mapcar (lambda (l)
                                   (cons element l))
                                 (permutations (remove element list)))))))

(def show-list^ls (list &key (output *standard-output*) (fn #'identity))
  "Display the items in LIST."
  (loop :for item :in list
        :do (format output "~S~%" (funcall fn item))))

;; from https://groups.google.com/g/comp.lang.lisp/c/1ZtO84hrAuM
(deft scramble ((sequence array))
  (loop :with len = (length sequence)
        :for i :from 0 :below len
        :do (rotatef (aref sequence i)
                     (aref sequence (+ (random (- len i)) i))))
  sequence)

(deft scramble ((sequence list))
  (coerce (scramble
           (make-array (length sequence) :initial-contents sequence))
          'list))

(def transpose (list)
  "Return a matrix transposition of `list'."
  (apply #'mapcar #'list list))
