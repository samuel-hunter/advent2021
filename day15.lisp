(defpackage #:advent2021.day15
  (:use #:cl #:alexandria #:cl-ppcre #:queues #:advent2021.util)
  (:export #:solve-part-1 #:solve-part-2))

(in-package #:advent2021.day15)



(defparameter +input+
  (loop :with lines := (parse-lines #'identity)
        :with array := (make-array (list (length lines) (length (first lines)))
                                   :element-type 'fixnum)
        :for line :in lines
        :for y :upfrom 0
        :do (loop :for c :across line
                  :for x :upfrom 0
                  :do (setf (aref array y x) (- (char-code c)
                                                (char-code #\0))))
        :finally (return array)))

(defparameter +full-map+
  (loop :with (height width) := (array-dimensions +input+)
        :with map := (make-array (list (* height 5) (* width 5))
                                 :element-type 'fixnum)
        :for x :below width
        :do (loop :for y :below height
                  :for risk := (aref +input+ y x)
                  :do (loop :for levelx :below 5
                            :for x* := (+ x (* width levelx))
                            :do (loop :for levely :below 5
                                      :for y* := (+ y (* height levely))
                                      :for risk* := (1+ (mod (+ risk levelx levely -1) 9))
                                      :do (setf (aref map y* x*) risk*))))
        :finally (return map)))

(defun route (x y risk)
  (list x y risk))

(defun risk (route)
  (third route))

(defun risk< (a b)
  (< (risk a)
     (risk b)))

(defun solve (risk-map)
  (let* ((width (array-dimension risk-map 1))
         (height (array-dimension risk-map 0))
         (route-queue (make-queue :priority-queue :compare #'risk<))
         (route-array (make-array (list height width)
                                 :element-type 'fixnum
                                 :initial-element most-positive-fixnum))
         (target (list (1- width) (1- height))))
    ;; Initialize the prio queue
    (setf (aref route-array 0 0) 0)
    (loop :for x :below width
          :do (loop :for y :below height
                    :do (qpush route-queue (route x y (if (and (zerop x)
                                                               (zerop y))
                                                          0
                                                          most-positive-fixnum)))))
    (flet ((qpush* (x y risk)
             (unless (or (< x 0)
                         (< y 0)
                         (>= x width)
                         (>= y height)
                         (>= (incf risk (aref risk-map y x))
                             (aref route-array y x)))
               (queue-delete route-queue
                             (queue-find route-queue
                                         (lambda (route)
                                           (and (= x (first route))
                                                (= y (second route))))))
               (qpush route-queue (route x y risk))
               (setf (aref route-array y x) risk))))
      (loop :for (x y risk) := (qpop route-queue)
            :do (if (equal (list x y) target)
                  (return risk)
                  (progn
                    (qpush* (1- x) y risk)
                    (qpush* (1+ x) y risk)
                    (qpush* x (1- y) risk)
                    (qpush* x (1+ y) risk)))))))

(defun solve-part-1 ()
  (solve +input+))

(defun solve-part-2 ()
  (solve +full-map+))

