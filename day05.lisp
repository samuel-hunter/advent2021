(defpackage #:advent2021.day05
  (:use #:cl #:alexandria #:cl-ppcre #:advent2021.util)
  (:export #:solve-part-1 #:solve-part-2))

(in-package #:advent2021.day05)



(defparameter +input+
  ;; (x1 y1 x2 y2)
  (parse-lines
    (lambda (line)
      (mapcan (lambda (s)
                (mapcar #'parse-integer (split "," s)))
              (split " -> " line)))))

(defun walk-line (function line)
  (destructuring-bind (x1 y1 x2 y2) line
    (loop :with dx := (signum (- x2 x1))
          :with dy := (signum (- y2 y1))

          :for x := x1 :then (+ x dx)
          :for y := y1 :then (+ y dy)

          :do (funcall function x y)
          :until (and (= x x2) (= y y2)))))

(defmacro do-line ((x y) line &body body)
  `(walk-line (lambda (,x ,y) ,@body) ,line))

(defun is-orthographic (line)
  (destructuring-bind (x1 y1 x2 y2) line
    (or (= x1 x2) (= y1 y2))))

(defun max-dimensions ()
  (loop :for (x1 y1 x2 y2) :in +input+
        :maximize (max x1 x2) :into x
        :maximize (max y1 y2) :into y
        :finally (return (list (1+ x) (1+ y)))))

(defun solve (count-diagonals)
  (loop :with (w h) := (max-dimensions)
        :with grid := (make-array (list h w) :element-type 'integer)
        :for line :in +input+
        :when (or (is-orthographic line) count-diagonals)
          :do (do-line (x y) line
                (incf (aref grid y x)))
        :finally (return
                   (loop :for x :below w
                         :sum (loop :for y :below h
                                    :count (> (aref grid y x) 1))))))

(defun solve-part-1 ()
  (solve nil))

(defun solve-part-2 ()
  (solve t))
