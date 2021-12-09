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

(defun adjacents (x y)
  (flet ((access (x y)
           (if (and (>= x 0)
                    (>= y 0)
                    (< x +width+)
                    (< y +height+))
               (aref +input+ y x)
               9999)))
    (list (access x (1- y))
          (access x (1+ y))
          (access (1- x) y)
          (access (1+ x) y))))

(defun low-point? (x y)
  (loop :with point := (aref +input+ y x)
        :for adjacent :in (adjacents x y)
        :always (< point adjacent)))

(defun solve-part-1 ()
  (loop :for y :below +height+
        :sum (loop :for x :below +width+
                   :when (low-point? x y)
                     :sum (1+ (aref +input+ y x)))))

(defun fill-basin (x y touch-array)
  (when (or (< x 0) (< y 0)
            (>= x +width+) (>= y +height+)
            (= 1 (aref touch-array y x))
            (= 9 (aref +input+ y x)))
    (return-from fill-basin 0))

  (setf (aref touch-array y x) 1)
  (+ 1
     (fill-basin (1- x) y touch-array)
     (fill-basin (1+ x) y touch-array)
     (fill-basin x (1- y) touch-array)
     (fill-basin x (1+ y) touch-array)))

(defun basin-sizes ()
  (loop :with touch-array := (make-array (list +height+ +width+)
                                         :element-type 'bit)
        :for y :below +height+
        :append (loop :for x :below +width+
                      :for score := (fill-basin x y touch-array)
                      :when (> score 0)
                      :collect score)))

(defun solve-part-2 ()
  (reduce #'* (subseq (sort (basin-sizes) #'>) 0 3)))
