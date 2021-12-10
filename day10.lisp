(defpackage #:advent2021.day10
  (:use #:cl #:alexandria #:cl-ppcre #:advent2021.util)
  (:export #:solve-part-1 #:solve-part-2))

(in-package #:advent2021.day10)



(defparameter +input+
  (parse-lines #'identity))

(defparameter +pairs+
  '(#\( #\) #\[ #\] #\{ #\} #\< #\>))

(defparameter +scores+
  '(#\) 3 #\] 57 #\} 1197 #\> 25137))

(defun score (line)
  (loop :with stack := ()
        :for char :across line
        :do (cond
              ((member char '(#\( #\[ #\{ #\<))
               (push char stack))
              ((eql char (getf +pairs+ (first stack)))
               (pop stack))
              (t (return (getf +scores+ char))))))

(defun solve-part-1 ()
  (loop :for line :in +input+
        :for score := (score line)
        :when score
          :collect score :into scores
        :finally (Return (reduce #'+ scores))))

(defun valid-lines ()
  (loop :for line :in +input+
        :for score := (Score line)
        :unless score
          :collect line))

(defun score* (line)
  (loop :with stack := ()
        :for char :across line
        :do (if (member char '(#\( #\[ #\{ #\<))
                (push char stack)
                (pop stack))
        :finally (return (loop :with score := 0
                               :for s :in stack
                               :for round := (1+ (position s '(#\( #\[ #\{ #\<)))
                               :do (setf score (+ (* score 5) round))
                               :finally (return score)))))

(defun scores ()
  (loop :for line :in (valid-lines)
        :for score := (score* line)
        :collect score))

(defun solve-part-2 ()
  (let* ((s (sort (scores) #'<))
         (l (length s)))
    (nth (floor l 2) s)))

