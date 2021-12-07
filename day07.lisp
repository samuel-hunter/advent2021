(defpackage #:advent2021.day07
  (:use #:cl #:alexandria #:cl-ppcre #:advent2021.util)
  (:export #:solve-part-1 #:solve-part-2))

(in-package #:advent2021.day07)



(defparameter +input+
  (first (parse-lines
           (lambda (line)
             (loop :with array := (make-array 0 :adjustable t)
                   :for x :in (mapcar #'parse-integer (split "," line))
                   :do (when (>= x (length array))
                         (adjust-array array (1+ x)))
                       (incf (aref array x))
                   :finally (return array))))))

(defun fuel-required (dest function)
  (loop :for n :across +input+
        :for x :upfrom 0
        :for distance := (abs (- x dest))
        :sum (* n (funcall function distance))))

(defun solve (function)
  (loop :with winner := nil
        :with lowest-score := nil
        :initially (setf winner 0
                         lowest-score (fuel-required 0 function))
        :for dest :from 1 :below (length +input+)
        :for fuel := (fuel-required dest function)
        :do (when (< fuel lowest-score)
              (setf winner dest
                    lowest-score fuel))
        :finally (return lowest-score)))

(defun solve-part-1 ()
  (solve #'identity))

(defun solve-part-2 ()
  (solve (lambda (x) (* x (1+ x) 1/2))))
