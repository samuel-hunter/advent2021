(defpackage #:advent2021.day02
  (:use #:cl #:alexandria #:advent2021.util)
  (:export #:solve-part-1 #:solve-part-2))

(in-package #:advent2021.day02)



(defparameter +input+ (read-puzzle-sexp))

(defparameter +input+ '(
                        (forward 5)
                        (down 5)
                        (forward 8)
                        (up 3)
                        (down 8)
                        (forward 2)))

(defun solve-part-2 ()
  (loop :with horiz := 0
        :with depth := 0
        :with aim := 0
        :for (cmd mag) :in +input+
        :do (cond
              ((eq cmd 'forward) (incf horiz mag)
                                 (incf depth (* aim mag))
                                 )
              ((eq cmd 'backward) (decf horiz mag))
              ((eq cmd 'up)
                            (decf aim mag))
              ((eq cmd 'down)
                              (incf aim mag)))
        :do (print (list horiz depth aim))
        :finally (return (* horiz depth))))(defun solve-part-1 ()

  (loop :with horiz := 0
        :with depth := 0
        :for (cmd mag) :in +input+
        :do (cond
              ((eq cmd 'forward) (incf horiz mag))
              ((eq cmd 'backward) (decf horiz mag))
              ((eq cmd 'up) (incf depth mag))
              ((eq cmd 'down) (decf depth mag)))
        :finally (return (* horiz depth))))

(defun solve-part-2 ()
  )

