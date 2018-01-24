;;;; matrix.lisp

;;; Utilities for working with finite two-dimensional matrices

(in-package :mof)

(defun index-string (count string)
  "Create data index from STRING prefixed with COUNT"
  (loop
     :for char :across string
     :for index = 0 :then (1+ index)
     :collect (list (list count index) char)))

(defun map-string (string separator)
  "Build a list of character index from STRING prefixed with their coordinates
separated by SEPARATOR"
  (loop
     :for word :in (split-string string separator)
     :for count = 0 :then (1+ count)
     :nconc (index-string count word)))

(defun build-lines (string separator)
  "Build a list of list of characters from words in LINE."
  (mapcar #'string-list (split-string string separator)))

(defun build-matrix (strings separator)
  "Build a matrix from STRING"
  (let ((hash (make-hash-table :test #'equal)))
    (loop
       :for (key val)
       :in (map-string strings separator)
       :do (setf (gethash key hash) val))
    hash))

(defun coordinates (matrix)
  "Return the list of coordinates of MATRIX"
  (loop :for k :being the hash-keys :in matrix :collect k))

(defun element (coordinate matrix)
  "Return the element under COORDINATE from MATRIX"
  (gethash coordinate matrix))

(defun elements (matrix)
  "Return the elements of MATRIX"
  (loop :for v :being the hash-values :in matrix :collect v))

(defun print-matrix (matrix)
  "View hash table processed by BUILD-MATRIX"
  (maphash #'(lambda (key value) (format t "~S: ~S~%" key value))
           matrix))

(defun dimensions (matrix)
  "Return the dimensions of MATRIX as two values"
  (let ((last-coordinate (last* (coordinates matrix))))
    (destructuring-bind (rows cols)
        last-coordinate
      (values (1+ rows)
              (1+ cols)))))

(defun coordinate-equal-p (a b)
  "Return true if coordinates A and B match"
  (if (or (null a) (null b))
      nil
      (destructuring-bind ((a-x a-y) (b-x b-y))
          (list a b)
        (when (and (= a-x b-x) (= a-y b-y))
            t))))

(defun valid-coordinate-p (coordinate matrix)
  "Return true if COORDINATE is part of MATRIX"
  (multiple-value-bind (var exists)
      (gethash coordinate matrix)
    (declare (ignore var))
    exists))

(defun adjacent-coordinates (coordinate)
  "Return a list of coordinates surrounding COORDINATE"
  (let* ((steps '(-1 0 1))
         (coordinates (remove
                       coordinate
                       (destructuring-bind (a b) coordinate
                         (collect list
                             ((list (+ a x) (+ b y)))
                           (in x steps)
                           (in y steps)))
                       :test #'equal)))
    coordinates))

(defun ensure-coordinate (coordinate matrix)
  "Return COORDINATE if it is part of MATRIX. Otherwise, return NIL"
  (and coordinate
       (if (valid-coordinate-p coordinate matrix)
           coordinate
           nil)))

(defun peek-up (coordinate)
  "Return the coordinate up, if it exists. Otherwise, return NIL"
  (destructuring-bind (x y) coordinate `(,(1- x) ,y)))

(defun peek-right (coordinate)
  "Return the coordinate to the right, if it exists. Otherwise, return NIL"
  (destructuring-bind (x y) coordinate `(,x ,(1+ y))))

(defun peek-down (coordinate)
  "Return the coordinate down, if it exists. Otherwise, return NIL"
  (destructuring-bind (x y) coordinate `(,(1+ x) ,y)))

(defun peek-left (coordinate)
  "Return the coordinate to the left, if it exists. Otherwise, return NIL"
  (destructuring-bind (x y) coordinate `(,x ,(1- y))))

(defun find-element-coordinates (element matrix)
  "Find all coordinates containing ELEMENT from MATRIX"
  (loop
     :for key :being the hash-keys :in matrix
     :for val :being the hash-values :in matrix
     :if (equal val element) :collect key))

(defun filter-lines (lines length)
  "Return LINES with length LENGTH"
  (loop :for line :in lines :when (= (length line) length) :collect line))

(defun line-values (line matrix)
  "Return LINE as characters"
  (loop :for char :in line :collect (element char matrix)))

(defun list-string (list)
  "Return LIST of characters as string"
  (format nil "~{~A~}" list))

(defun line-string (line matrix)
  "Return LINE from MATRIX as string"
  (list-string (line-values line matrix)))


;;; ?
(defun range (start end)
  "Return a list of coordinates from START to END, inclusive"
  (destructuring-bind ((x1 y1) (x2 y2))
      (list start end)
    (cond ((= x1 x2) (loop :for i :from y1 :to y2 :collect (list x1 i)))
          ((= y1 y2) (loop :for i :from x1 :to x2 :collect (list i y1)))
          (t nil))))

(defun sort-coordinates (coordinates)
  "Sort COORDINATES in increasing order"
  (sort coordinates
        #'(lambda (a-coordinate b-coordinate)
            (destructuring-bind ((x1 y1) (x2 y2))
                (list a-coordinate b-coordinate)
              (and (<= x1 x2) (<= y1 y2))))))

(defun remove-duplicate-coordinates (coordinates)
  "Remove duplicates coordinates in COORDINATES"
  (labels ((fn (coords acc)
             (cond ((or (null (cdr coords)))
                    (reverse acc))
                   ((coordinate-equal-p (first coords) (second coords))
                    (fn (cddr coords) (cons (first coords) acc)))
                   (t (fn (cdr coords) (cons (first coords) acc))))))
    (fn coordinates nil)))

(defun boundaries (start end)
  "Return the list of boundary coordinates from START to END"
  (let* ((top (range start (list (first start) (second end))))
         (left (range start (list (first end) (second start))))
         (end1 (first (last top)))
         (end2 (first (last left)))
         (right (range end1 end))
         (bottom (range end2 end)))
    (remove-duplicate-coordinates
     (sort-coordinates
      (append top left right bottom)))))
