(defpackage #:advent2021.day13
  (:use #:cl #:alexandria #:cl-ppcre #:advent2021.util)
  (:export #:solve-part-1 #:solve-part-2))

(in-package #:advent2021.day13)



(with-puzzle-file (stream)
  (defparameter +paper+
    (loop :with paper := (make-array '(0 0)
                                     :element-type 'bit
                                     :adjustable t)
          :for line := (read-line stream)
          :until (string= line "")
          :for (x y) := (mapcar #'parse-integer
                           (split "," line))
          :for (height width) := (array-dimensions paper)
          :do (progn
                (adjust-array paper (list (max height (1+ y))
                                          (max width (1+ x))))
                (setf (aref paper y x) 1))
          :finally (return paper)))

  (defparameter +folds+
    (loop :for line := (read-line stream nil)
          :while line
          :for line* := (subseq line (length "fold along "))
          :for (pos n) := (split "=" line*)
          :collect (list (intern (string-upcase pos) :keyword)
                         (parse-integer n)))))

(defun fold-x (paper axis)
  (loop :with (height width) := (array-dimensions paper)
        :with new-paper := (make-array (list height axis)
                                       :element-type 'bit)
        :for y :below height
        :do (loop :for x :below axis
                  :for reflected-x := (+ axis (- axis x))

                  :do (setf (aref new-paper y x)
                            (logior (aref paper y x)
                                    (if (< reflected-x width)
                                        (aref paper y (+ axis (- axis x)))
                                        0
                                        ))))
        :finally (return new-paper)))

(defun fold-y (paper axis)
  (loop :with (height width) := (array-dimensions paper)
        :with new-paper := (make-array (list axis width)
                                       :element-type 'bit)
        :for y :below axis
        :for reflected-y := (+ axis (- axis y))
        :do (loop :for x :below width
                  :do (setf (aref new-paper y x)
                            (logior (aref paper y x)
                                    (if (< reflected-y height)
                                        (aref paper reflected-y x)
                                        0))))
        :finally (return new-paper)))

(defun solve-part-1 ()
  (loop :with paper := (fold-x +paper+ 655)
        :with (height width) := (array-dimensions paper)
        :for y :below height
        :sum (loop :for x :below width
                   :sum (aref paper y x))))

(defun print-pape (paper)
  (loop :with (height width) := (array-dimensions paper)
        :for y :below height
        :do (loop :for x :below width
                  :for val := (aref paper y x)
                  :do (write-char (if (= val 1)
                                      #\X
                                      #\Space)))
        (terpri)))

(defun solve-part-2 ()
  (loop :with paper := +paper+
        :for (basis axis) :in +folds+
        :do (setf paper
                  (ecase basis
                    (:x (fold-x paper axis))
                    (:y (fold-y paper axis))))
        :finally (print paper)))

