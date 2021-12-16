(defpackage #:advent2021.day16
  (:use #:cl #:alexandria #:advent2021.util)
  (:export #:solve-part-1 #:solve-part-2))

(in-package #:advent2021.day16)



(defun hex2val (c)
  (if (<= (char-code #\0) (char-code c) (char-code #\9))
      (- (char-code c) (char-code #\0)) ;; 0..9
      (- (char-code (char-upcase c)) (char-code #\A) -10))) ;; A..F

(defparameter +input+
  ;; Flatten the hex puzzle input into a bit-array.
  (loop :with line := (with-puzzle-file (stream)
                        (read-line stream))
        :with array := (make-array (* 4 (length line))
                                   :element-type 'bit)
        :for c :across line
        :for nybble := (hex2val c)
        :for i :upfrom 0 :by 4
        :do (setf (aref array i) (logand 1 (ash nybble -3))
                  (aref array (+ 1 i)) (logand 1 (ash nybble -2))
                  (aref array (+ 2 i)) (logand 1 (ash nybble -1))
                  (aref array (+ 3 i)) (logand 1 (ash nybble -0)))
        :finally (return array)))

(defconstant +literal-type+ 4)

(defmacro defgetf-accessor (name indicator)
  "Define an inline-desirable wrapper function around getf."
  `(progn
     (declaim (inline ,name))
     ;; XXX this macro leaks symbols NEW-VALUE and PLACE to the INDICATOR
     ;; param. I'd rather not change it to use gensyms though, because I want
     ;; those symbol names to be present in the function documentation, and the
     ;; indicators are only keywords in practice anyways.
     (defun ,name (place)
       (getf place ,indicator))
     (defun (setf ,name) (new-value place)
       (setf (getf place ,indicator) new-value))))

(defun make-reader ()
  (list :pos 0))

(defgetf-accessor pos :pos)

(defun make-packet (version type payload)
  (list :version version :type type
        (if (= type +literal-type+)
            :value :subpackets) payload))

(defgetf-accessor ptype :type)
(defgetf-accessor version :version)
(defgetf-accessor value :value)
(defgetf-accessor subpackets :subpackets)

(defun read-bits (reader size)
  (loop :with start := (pos reader)
        :with result := 0
        :for i :from start :below (+ start size)
        :do (setf result (logior (ash result 1)
                                 (aref +input+ i)))
        :finally (incf (pos reader) size)
                 (return result)))

(defun read-literal (reader)
  (loop :with result := 0
        :for group := (read-bits reader 5)
        :do (setf result (logior (ash result 4)
                                 (logand group #b1111)))
        :while (= (logand #b10000 group) #b10000)
        :finally (return result)))

(defun read-packet (reader)
  (let ((version (read-bits reader 3))
        (type (read-bits reader 3)))
    (cond
      ;; simple (literal-value) packet:
      ((= type 4)
       (make-packet version type (read-literal reader)))
      ;; sized compound packet:
      ((= 0 (read-bits reader 1))
       (make-packet version type
                    (loop :with end := (+ (read-bits reader 15)
                                          (pos reader))
                          :collect (read-packet reader)
                          :until (= (pos reader) end)
                          :do (assert (not (> (pos reader) end))))))
      ;; sizeless compound packet:
      (t (make-packet version type
                      (loop :repeat (read-bits reader 11)
                            :collect (read-packet reader)))))))

(defun sum-versions (packet)
  (+ (version packet)
     (reduce #'+ (mapcar #'sum-versions (subpackets packet)))))

(defun solve-part-1 ()
  (sum-versions (read-packet (make-reader))))

(defun eval-packet (packet)
  (etypecase (ptype packet)
    ;; n-ary arithmetic compound packet:
    ((integer 0 3)
     (reduce (nth (ptype packet)
                  (list #'+ #'* #'min #'max))
             (mapcar #'eval-packet (subpackets packet))))
    ;; simple (literal-value) packet:
    ((eql 4)
     (value packet))
    ;; binary test compound packet:
    ((integer 5 7)
     (if (funcall (nth (- (ptype packet) 5)
                       (list #'> #'< #'=))
                  (eval-packet (first (subpackets packet)))
                  (eval-packet (second (subpackets packet))))
         1 0))))

(defun solve-part-2 ()
  (eval-packet (read-packet (make-reader))))
