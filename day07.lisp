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

(defun fuel (dest)
  (loop :for n :across +input+
        :for x :upfrom 0
        :sum (* n (abs (- x dest)))))

(defun fuel2 (dest)
  (loop :for n :across +input+
        :for x :upfrom 0
        :for dist := (abs (- x dest))
        :for fuel-per := (abs (/ (* dist (1+ dist)) 2))
        :sum (* n fuel-per)))

(defun solve-part-1 ()
  (loop :with winner := -1
        :with score := 99999999999
        :for i :below (length +input+)
        :for fuel := (fuel i)
        :do (print (list :x i '= fuel))
        :do (when (<= fuel score)
              (setf score fuel
                    winner i))
        :finally (return score)))

(defun solve-part-2 ()
  (loop :with winner := -1
        :with score := 99999999999
        :for i :below (length +input+)
        :for fuel := (fuel2 i)
        :do (print (list :x i '= fuel))
        :do (when (<= fuel score)
              (setf score fuel
                    winner i))
        :finally (return (list i score))))

(defun solve-part-2 ()
  )
