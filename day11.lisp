(defpackage #:advent2021.day11
  (:use #:cl #:alexandria #:cl-ppcre #:advent2021.util)
  (:export #:solve-part-1 #:solve-part-2))

(in-package #:advent2021.day11)



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

(defun tick (grid x y touch-array)
  (when (or (< x 0)
            (< y 0)
            (>= x +width+)
            (>= y +height+)
            (= 1 (aref touch-array y x)))
    (return-from tick))
  (incf (aref grid y x))
  (when (> (aref grid y x) 9)
    (setf (aref touch-array y x) 1)
    (tick grid (1- x) (1- y) touch-array)
    (tick grid (1- x) y touch-array)
    (tick grid (1- x) (1+ y) touch-array)
    (tick grid x (1- y) touch-array)
    (tick grid x (1+ y) touch-array)
    (tick grid (1+ x) (1- y) touch-array)
    (tick grid (1+ x) y touch-array)
    (tick grid (1+ x) (1+ y) touch-array)))

(defun tick-all (grid touch-array)
  (loop :for x :below +width+
        :do (loop :for y :below +height+
                  :do (tick grid x y touch-array))))

(defun clear-touches (grid touch-array)
  (loop :for x :below +width+
        :sum (loop :for y :below +height+
                   :count (when (= 1 (aref touch-array y x))
                            (setf (aref grid y x) 0)
                            (setf (aref touch-array y x) 0)
                            t))))

(defun solve-part-1 ()
  (loop :with grid := (copy-array +input+)
        :with touch-array := (make-array (list +height+ +width+)
                                         :element-type 'bit)
        :with flashes := 0
        :repeat 100
        :do (tick-all grid touch-array)
            (incf flashes (clear-touches grid touch-array))
        :finally (Return flashes)))

(defun solve-part-2 ()
  (loop :with grid := (copy-array +input+)
        :with touch-array := (make-array (list +height+ +width+)
                                         :element-type 'bit)
        :for generations :upfrom 1
        :do (tick-all grid touch-array)
        :until (= (clear-touches grid touch-array) (* +width+ +height+))
          :finally (return generations)))
