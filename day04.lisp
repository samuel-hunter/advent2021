(defpackage #:advent2021.day04
  (:use #:cl #:alexandria #:cl-ppcre #:advent2021.util)
  (:export #:solve-part-1 #:solve-part-2))

(in-package #:advent2021.day04)



(defparameter +nums+
  '(67 31 58 8 79 18 19 45 38 13 40 62 85 10 21 96 56 55 4 36 76 42 32 34 39 89
    6 12 24 57 93 47 41 52 83 61 5 37 28 15 86 23 69 92 70 27 25 53 44 80 65 22
    99 43 66 26 11 72 2 98 14 82 87 20 73 46 35 7 1 84 95 74 81 63 78 94 16 60
    29 97 91 30 17 54 68 90 71 88 77 9 64 50 0 49 48 75 3 59 51 33))

(defparameter +input+
  (parse-forms
    (lambda (lines)
        ;; Parse forms of boards as 5x5 arrays of integers
        (loop :with board := (make-array '(5 5))
            :for line :in lines
            :for y :upfrom 0
            :do (loop :for n :in (split "\\s+" (string-trim " " line))
                      :for x :upfrom 0
                      :do (setf (aref board y x)
                                (parse-integer n)))
            :finally (return board)))))

(defun mark-board (board num)
  "Replace all instances of NUM in BOARD with T."
  (loop :for y :below 5
        :do (loop :for x :below 5
                  :when (eql (aref board y x) num)
                    :do (setf (aref board y x) t))))

(defun winning? (board)
  "Return whether BOARD has a running row or column of T's."
  (loop :for line :below 5
        :thereis (or (loop :for n :below 5
                           :always (eq (aref board n line) t))
                     (loop :for n :below 5
                           :always (eq (aref board line n) t)))))

(defun board-score (board)
  "Sum all unmarked numbers of BOARD."
  (loop :for x :below 5
        :sum (loop :for y :below 5
                   :for n := (aref board y x)
                   :sum (if (numberp n) n 0))))

(defun solve-part-1 ()
  (loop :with boards := (mapcar #'copy-array +input+)
        :for num :in +nums+
        :do (dolist (board boards)
              (mark-board board num))
        :do (loop :for board :in boards
                  :when (winning? board)
                    :do (return-from solve-part-1
                                     (* num (board-score board))))))

(defun solve-part-2 ()
  (loop :with boards := (mapcar #'copy-array +input+)
        :for num :in +nums+
        :do (dolist (board boards)
              (mark-board board num))
        :do (loop :for board :in boards
                  :when (winning? board)
                    :do (if (= 1 (length boards))
                            (return-from solve-part-2
                                         (* num (board-score board)))
                            (setf boards (delete board boards))))))
