(defpackage #:advent2021.day02
  (:use #:cl #:alexandria #:split-sequence #:advent2021.util)
  (:export #:solve-part-1 #:solve-part-2))

(in-package #:advent2021.day02)



(defparameter +input+
  (parse-lines
    (lambda (line)
      (destructuring-bind (cmd arg) (split-sequence #\Space line)
        (cons (intern (string-upcase cmd) :keyword)
              (parse-integer arg))))))

(defun solve-part-1 ()
  (loop :with breadth := 0
        :with depth := 0
        :for (cmd . mag) :in +input+
        :do (ecase cmd
              (:down (incf depth mag))
              (:up (decf depth mag))
              (:forward (incf breadth mag)))
        :finally (return (* depth breadth))))

(defun solve-part-2 ()
  (loop :with aim := 0
        :with breadth := 0
        :with depth := 0
        :for (cmd . mag) :in +input+
        :do (ecase cmd
              (:down (incf aim mag))
              (:up (decf aim mag))
              (:forward
                (incf breadth mag)
                (incf depth (* aim mag))))
        :finally (return (* depth breadth))))
