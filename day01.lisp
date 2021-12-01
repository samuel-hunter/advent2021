(defpackage #:advent2021.day01
  (:use #:cl #:alexandria #:advent2021.util)
  (:export #:solve-part-1 #:solve-part-2))

(in-package #:advent2021.day01)



(defparameter +input+ (parse-lines #'parse-integer))

(defun solve-part-1 ()
  (loop :for (a b) :on +input+
        :while b
        :count (> b a)))

(defun sliding-sums ()
  (loop :for (a b c) :on +input+
        :while c
        :collect (+ a b c)))

(defun solve-part-2 ()
  (loop :for (a b) :on (sliding-sums)
        :while b
        :count (> b a)))
