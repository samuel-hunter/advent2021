(defpackage #:advent2021.day09
  (:use #:cl #:alexandria #:cl-ppcre #:advent2021.util)
  (:export #:solve-part-1 #:solve-part-2))

(in-package #:advent2021.day09)



(defparameter +input+
  (loop :with lines := (parse-lines #'identity)
        :with array := (make-array (list (length lines) (length (first lines))))
        :for line :in lines
        :for y :upfrom 0
        :do (loop :for c :across line
                  :for x :upfrom 0
                  :do (setf (aref array y x) (- (char-code c)
                                                (char-code #\0))))
        :finally (return array)))

(defparameter +width+ (array-dimension +input+ 1))
(defparameter +height+ (array-dimension +input+ 0))

(defun height (x y)
  (if (and (>= x 0)
           (>= y 0)
           (< x +width+)
           (< y +height+))
      (aref +input+ y x)
      9))

(defun low-point? (x y)
  (loop :with point := (height x y)
        :for adjacent :in (list (height (1- x) y)
                                (height (1+ x) y)
                                (height x (1- y))
                                (height x (1+ y)))
        :always (< point adjacent)))

(defun solve-part-1 ()
  (loop :for y :below +height+
        :sum (loop :for x :below +width+
                   :when (low-point? x y)
                     :sum (1+ (aref +input+ y x)))))

(defun fill-basin (x y touch-array)
  "Fill the basin located at the given point, marking it as already visited on the touch-array and returning the baisn's size."
  (when (or (= 9 (height x y))
            (= 1 (aref touch-array y x)))
    (return-from fill-basin 0))

  ;; Good ol' Fill Algorithm
  (setf (aref touch-array y x) 1)
  (+ 1
     (fill-basin (1- x) y touch-array)
     (fill-basin (1+ x) y touch-array)
     (fill-basin x (1- y) touch-array)
     (fill-basin x (1+ y) touch-array)))

(defun basin-sizes ()
  "Return an unordered list of all basin sizes"
  (loop :with touch-array := (make-array (list +height+ +width+)
                                         :element-type 'bit)
        :for y :below +height+
        :append (loop :for x :below +width+
                      :for score := (fill-basin x y touch-array)
                      :when (> score 0)
                        :collect score)))

(defun solve-part-2 ()
  "Multiply the largest 3 basin sizes."
  (reduce #'* (subseq (sort (basin-sizes) #'>)
                      0 3)))
