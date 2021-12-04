(defpackage #:advent2021.day04
  (:use #:cl #:alexandria #:split-sequence #:advent2021.util)
  (:export #:solve-part-1 #:solve-part-2))

(in-package #:advent2021.day04)



(defparameter +nums+ '(67 31 58 8 79 18 19 45 38 13 40 62 85 10 21 96 56 55 4 36 76 42 32 34 39 89 6 12 24 57 93 47 41 52 83 61 5 37 28 15 86 23 69 92 70 27 25 53 44 80 65 22 99 43 66 26 11 72 2 98 14 82 87 20 73 46 35 7 1 84 95 74 81 63 78 94 16 60 29 97 91 30 17 54 68 90 71 88 77 9 64 50 0 49 48 75 3 59 51 33))

(defparameter +input+
  (parse-forms
    (lambda (lines)
      (loop :with board := (make-array '(5 5))
            :for line :in lines
            :for y :upfrom 0
            :do (loop :for n :in (split-sequence #\Space line)
                      :for x :upfrom 0
                      :do (setf (aref board y x )
                                (parse-integer n)))
            :finally (return board)))))

(defun update-board (board num)
  (loop :for y :below 5
        :do (loop :for x :below 5
                  :when (eql (aref board y x) num)
                    :do (setf (aref board y x) t)
                        (return-from update-board))))

(defun winning? (board)
  (or
    (loop :for x :below 5
          :thereis (loop :for y :below 5
                    :always (eq (aref board y x) t)))
    (loop :for y :Below 5
          :thereis (loop :for x :below 5
                    :always (eq (aref board y x) t)))))

(defun board-score (board)
  (loop :for x :below 5
        :sum (loop :for y :below 5
                   :for n := (aref board y x)
                   :sum (if (numberp n) n 0))))

(defun solve-part-1 ()
  (loop :with boards := (mapcar #'copy-array +input+)
        :initially (print boards)
        :for num :in +nums+
        :do (dolist (board boards)
              (update-board board num))
        :do (print num)
        :do (print boards)
        :do (loop :for board :in boards
                  :for boardi :upfrom 1
                  :when (winning? board)
                  :do (print boardi) (print board)
                      (return-from solve-part-1
                                   (* num (board-score board))))))

(defun playing-boards-left (boards)
  (loop :for board :in boards
        :count (not (winning? board))))

(defun last-playing-board (Boards)
  (loop :for board :in boards
        :while (winning? board)
        :finally (return board))
  )

(defun solve-part-2 ()
  (loop :with boards := (mapcar #'copy-array +input+)
        :for num :in +nums+
        :do (dolist (board boards)
              (update-board board num))
        :do (loop :for board :in boards
                  :when (winning? board)
                  :do (if (= 1 (length boards))
                          (return-from solve-part-2
                                       (* num (board-score board)))
                          (setf boards (delete board boards))))))
