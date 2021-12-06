(defpackage #:advent2021.day06
  (:use #:cl #:alexandria #:cl-ppcre #:advent2021.util)
  (:export #:solve-part-1 #:solve-part-2))

(in-package #:advent2021.day06)



(defparameter +input+
  ;; array of # of fish in each state
  (first (parse-lines
           (lambda (line)
             (loop :with fishes := (make-array 9)
                   :for n :in (mapcar #'parse-integer (split "," line))
                   :do (incf (aref fishes n))
                   :finally (return fishes))))))

(defun tick-fish (fish)
  (rotatef (aref fish 0)
           (aref fish 1)
           (aref fish 2)
           (aref fish 3)
           (aref fish 4)
           (aref fish 5)
           (aref fish 6)
           (aref fish 7)
           (aref fish 8))
  (incf (aref fish 6)
        (aref fish 8)))

(defun solve (generations)
  (let ((fish (copy-array +input+)))
    (dotimes (i generations)
      (tick-fish fish))

    ;; return total fish
    (loop :for n :across fish
          :sum n)))

(defun solve-part-1 ()
  (solve 80))

(defun solve-part-2 ()
  (solve 256))
