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

;; Brute-force all combinations, time typing was cheaper than time thinking
;; about a more elegant way to do this.
(defun walk-line (function line count-diagonals)
  (destructuring-bind (x1 y1 x2 y2) line
    (cond
      ((and (= x1 x2) (<= y1 y2))
       (loop :for y :from y1 :to y2
             :do (funcall function x1 y)))
      ((= x1 x2)
       (loop :for y :from y1 :downto y2
             :do (funcall function x1 y)))
      ((and (= y1 y2) (<= x1 x2))
       (loop :for x :from x1 :to x2
             :do (funcall function x y1)))
      ((= y1 y2)
       (loop :for x :from x1 :downto x2
             :do (funcall function x y1)))
      (count-diagonals) ;; skip diagonals here
      ((and (<= x1 x2) (<= y1 y2))
       (loop :for x :from x1 :to x2
             :for y :from y1 :to y2
             :do (funcall function x y)))
      ((and (<= x1 x2))
       (loop :for x :from x1 :to x2
             :for y :from y1 :downto y2
             :do (funcall function x y)))
      ((and (<= y1 y2))
       (loop :for x :from x1 :downto x2
             :for y :from y1 :to y2
             :do (funcall function x y)))
      (t
       (loop :for x :from x1 :downto x2
             :for y :from y1 :downto y2
             :do (funcall function x y))))))

(defun max-dimensions ()
  (loop :for (x1 y1 x2 y2) :in +input+
        :maximize (max x1 x2) :into x
        :maximize (max y1 y2) :into y
        :finally (return (list (1+ x) (1+ y)))))

(defun solve (count-diags)
  (loop :with (w h) := (max-dimensions)
        :with grid := (make-array (list h w) :element-type 'integer)
        :for line :in +input+
        :do (walk-line (lambda (x y)
                         (incf (aref grid y x)))
                       line count-diags)
        :finally (return
                   (loop :for x :below w
                         :sum (loop :for y :below h
                                    :count (> (aref grid y x) 1))))))

(defun solve-part-1 ()
  (solve t))

(defun solve-part-2 ()
  (solve nil))
