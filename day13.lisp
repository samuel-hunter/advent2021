(defpackage #:advent2021.day13
  (:use #:cl #:alexandria #:cl-ppcre #:advent2021.util)
  (:export #:solve-part-1 #:solve-part-2))

(in-package #:advent2021.day13)



(defun make-paper (width height)
  (make-array (list height width) :element-type 'bit))

(defun dot (paper x y)
  (destructuring-bind (height width) (array-dimensions paper)
    (if (and (< x width)
             (< y height))
        (aref paper y x)
        0)))

(defun (setf dot) (new-value paper x y)
  (setf (aref paper y x) new-value))

(defun parse-dot (line)
  (mapcar #'parse-integer (split "," line)))

(defun read-dimensions (stream)
  (loop :with old-position := (file-position stream)
        :for line := (read-line stream)
        :until (string= line "")
        :for (x y) := (parse-dot line)
        :maximize x :into max-x
        :maximize y :into max-y
        :finally (progn
                   (file-position stream old-position)
                   (return (list (1+ max-x) (1+ max-y))))))

(with-puzzle-file (stream)
  (defparameter +paper+
    (loop :with (width height) := (read-dimensions stream)
          :with paper := (make-paper width height)
          :for line := (read-line stream)
          :until (string= line "")
          :for (x y) := (parse-dot line)
          :do (setf (dot paper x y) 1)
          :finally (return paper)))

  (defparameter +folds+
    (loop :for line := (read-line stream nil)
          :while line
          :for line* := (subseq line (length "fold along "))
          :for (pos n) := (split "=" line*)
          :collect (list (intern (string-upcase pos) :keyword)
                         (parse-integer n)))))

(defun fold-x (paper new-width)
  (loop :with height := (array-dimension paper 0)
        :with new-paper := (make-paper new-width height)

        :for y :below height
        :do (loop :for x :below new-width
                  :for reflected-x := (+ new-width new-width (- x))
                  :do (setf (aref new-paper y x)
                            (logior (dot paper x y)
                                    (dot paper reflected-x y))))
        :finally (return new-paper)))

(defun fold-y (paper new-height)
  (loop :with width := (array-dimension paper 1)
        :with new-paper := (make-paper width new-height)

        :for y :below new-height
        :for reflected-y := (+ new-height new-height (- y))
        :do (loop :for x :below width
                  :do (setf (dot new-paper x y)
                            (logior (dot paper x y)
                                    (dot paper x reflected-y))))
        :finally (return new-paper)))

(defun fold (paper axis dimension)
  (ecase axis
    (:x (fold-x paper dimension))
    (:y (fold-y paper dimension))))

(defun solve-part-1 ()
  (loop :with (axis dimension) := (first +folds+)
        :with paper := (fold +paper+ axis dimension)
        :with (height width) := (array-dimensions paper)

        :for y :below height
        :sum (loop :for x :below width
                   :sum (dot paper x y))))

(defun print-paper (paper)
  (loop :with (height width) := (array-dimensions paper)
        :for y :below height
        :do (loop :for x :below width
                  :for val := (aref paper y x)
                  :do (write-char (if (= val 1) #\X #\Space)))
            (terpri)))

(defun solve-part-2 ()
  (loop :with paper := +paper+
        :for (axis dimension) :in +folds+
        :do (setf paper (fold paper axis dimension))
        :finally (print-paper paper))
  (values))

