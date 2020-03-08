;;;; grids.lisp - utilities for working with finite two-dimensional grids

(uiop:define-package #:marie/grid
  (:use #:cl)
  (:export #:index-string
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

(defun index-string (count string)
  "Create data index from STRING prefixed with COUNT."
  (loop :for char :across string
        :for index = 0 :then (1+ index)
        :collect (list (list count index) char)))

(defun map-string (string separator)
  "Build a list of character index from STRING prefixed with their coordinates
separated by SEPARATOR."
  (loop :for word :in (split-string string separator)
        :for count = 0 :then (1+ count)
        :nconc (index-string count word)))

(defun build-lines (string separator)
  "Build a list of list of characters from words in LINE."
  (mapcar #'string-list (split-string string separator)))

(defun build-grid (strings separator)
  "Build a grid from STRING."
  (let ((hash (make-hash-table :test #'equal)))
    (loop :for (key val)
          :in (map-string strings separator)
          :do (setf (gethash key hash) val))
    hash))

(defun coordinates (grid)
  "Return the list of coordinates of GRID."
  (loop :for k :being the hash-keys :in grid :collect k))

(defun element (coordinate grid)
  "Return the element under COORDINATE from GRID."
  (gethash coordinate grid))

(defun elements (grid)
  "Return the elements of GRID."
  (loop :for v :being the hash-values :in grid :collect v))

(defun print-grid (grid)
  "View hash table processed by BUILD-GRID."
  (maphash #'(lambda (key value) (format t "~S: ~S~%" key value))
           grid))

(defun dimensions (grid)
  "Return the dimensions of GRID as two values."
  (let ((last-coordinate (last* (coordinates grid))))
    (destructuring-bind (rows cols)
        last-coordinate
      (values (1+ rows)
              (1+ cols)))))

(defun coordinate-equal-p (a b)
  "Return true if coordinates A and B match."
  (if (or (null a) (null b))
      nil
      (destructuring-bind ((a-x a-y) (b-x b-y))
          (list a b)
        (when (and (= a-x b-x) (= a-y b-y))
            t))))

(defun valid-coordinate-p (coordinate grid)
  "Return true if COORDINATE is part of GRID."
  (multiple-value-bind (var exists)
      (gethash coordinate grid)
    (declare (ignore var))
    exists))

(defun adjacent-coordinates (coordinate &key (include nil))
  "Return a list of coordinates surrounding COORDINATE."
  (let* ((steps '(-1 0 1))
         (coordinates (destructuring-bind (a b) coordinate
                        (collect list
                            ((list (+ a x) (+ b y)))
                          (in x steps)
                          (in y steps)))))
    (if include
        coordinates
        (remove coordinate coordinates :test #'equal))))

(defun cross-adjacent-coordinates (coordinate)
  "Return the adjacent coordinates of COORDINATE horizontally and vertically, only."
  (list (peek-left coordinate)
        (peek-up coordinate)
        (peek-right coordinate)
        (peek-down coordinate)))

(defun ensure-coordinate (coordinate grid)
  "Return COORDINATE if it is part of GRID. Otherwise, return NIL."
  (and coordinate
       (if (valid-coordinate-p coordinate grid)
           coordinate
           nil)))

(defun peek-up (coordinate)
  "Return the coordinate up, if it exists. Otherwise, return NIL."
  (destructuring-bind (x y) coordinate `(,(1- x) ,y)))

(defun peek-right (coordinate)
  "Return the coordinate to the right, if it exists. Otherwise, return NIL."
  (destructuring-bind (x y) coordinate `(,x ,(1+ y))))

(defun peek-down (coordinate)
  "Return the coordinate down, if it exists. Otherwise, return NIL."
  (destructuring-bind (x y) coordinate `(,(1+ x) ,y)))

(defun peek-left (coordinate)
  "Return the coordinate to the left, if it exists. Otherwise, return NIL."
  (destructuring-bind (x y) coordinate `(,x ,(1- y))))

(defun find-element-coordinates (element grid)
  "Find all coordinates containing ELEMENT from GRID."
  (loop :for key :being the hash-keys :in grid
        :for val :being the hash-values :in grid
        :if (equal val element) :collect key))

(defun filter-lines (lines length)
  "Return LINES with length LENGTH."
  (loop :for line :in lines :when (= (length line) length) :collect line))

(defun line-values (line grid)
  "Return LINE as characters."
  (loop :for char :in line :collect (element char grid)))

(defun list-string (list)
  "Return LIST of characters as string."
  (format nil "~{~A~}" list))

(defun line-string (line grid)
  "Return LINE from GRID as string."
  (list-string (line-values line grid)))

(defun last-coordinate (grid)
  "Return the last coordinate of GRID."
  (last* (coordinates grid)))

(defun grid-rows (grid)
  "Return the number of rows from GRID."
  (destructuring-bind (row column)
      (last-coordinate grid)
    (declare (ignore column))
    (1+ row)))

(defun grid-columns (grid)
  "Return the number of rows from GRID."
  (destructuring-bind (row column)
      (last-coordinate grid)
    (declare (ignore row))
    (1+ column)))

(defun group-coordinates (grid)
  "Create groups of coordinates according to the dimensions of GRID."
  (partition (coordinates grid) (grid-columns grid)))

(defun group-elements (grid)
  "Create groups of elements according to the dimensions of GRID."
  (partition (elements grid) (grid-columns grid)))


;;; ?
(defun range (start end)
  "Return a list of coordinates from START to END, inclusive."
  (destructuring-bind ((x1 y1) (x2 y2))
      (list start end)
    (cond ((= x1 x2) (loop :for i :from y1 :to y2 :collect (list x1 i)))
          ((= y1 y2) (loop :for i :from x1 :to x2 :collect (list i y1)))
          (t nil))))

(defun sort-coordinates (coordinates)
  "Sort COORDINATES in increasing order."
  (sort coordinates
        #'(lambda (a-coordinate b-coordinate)
            (destructuring-bind ((x1 y1) (x2 y2))
                (list a-coordinate b-coordinate)
              (and (<= x1 x2) (<= y1 y2))))))

(defun remove-duplicate-coordinates (coordinates)
  "Remove duplicates coordinates in COORDINATES."
  (labels ((fn (coords acc)
             (cond ((or (null (cdr coords)))
                    (reverse acc))
                   ((coordinate-equal-p (first coords) (second coords))
                    (fn (cddr coords) (cons (first coords) acc)))
                   (t (fn (cdr coords) (cons (first coords) acc))))))
    (fn coordinates nil)))

(defun boundaries (start end)
  "Return the list of boundary coordinates from START to END."
  (let* ((top (range start (list (first start) (second end))))
         (left (range start (list (first end) (second start))))
         (end1 (first (last top)))
         (end2 (first (last left)))
         (right (range end1 end))
         (bottom (range end2 end)))
    (remove-duplicate-coordinates
     (sort-coordinates
      (append top left right bottom)))))

