(defpackage #:advent2021.day16
  (:use #:cl #:alexandria #:cl-ppcre #:queues #:advent2021.util)
  (:export #:solve-part-1 #:solve-part-2))

(in-package #:advent2021.day16)



(defun hex2val (c)
  (if (<= (char-code #\0) (char-code c) (char-code #\9))
      (- (char-code c) (char-code #\0))
      (- (char-code (char-upcase c)) (char-code #\A) -10)))

(defparameter +input+
  (loop :with line := (with-puzzle-file (stream)
                        (read-line stream))
        :with array := (make-array (* 4 (length line))
                                   :element-type 'bit)
        :for c :across line
        :for nybble := (hex2val c)
        :for i :upfrom 0 :by 4
        :do (setf (aref array i) (logand 1 (ash nybble -3)))
        :do (setf (aref array (+ 1 i)) (logand 1 (ash nybble -2)))
        :do (setf (aref array (+ 2 i)) (logand 1 (ash nybble -1)))
        :do (setf (aref array (+ 3 i)) (logand 1 (ash nybble -0)))
        :finally (return array)))

(defun reader ()
  (cons 0 nil))

(defun pos (reader)
  (car reader))

(defun (setf pos) (new-value reader)
  (setf (car reader) new-value))

(defun read-bits (reader size)
  (loop :with start := (pos reader)
        :with result := 0
        :for i :from start :below (+ start size)
        :do (setf result (+ (ash result 1)
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

(defun read-subpackets-to-length (reader length)
  (loop :with start := (pos reader)
        :collect (read-packet reader)
        :while (< (- (pos reader) start) length)))

(defun read-packet (reader)
  (let ((version (read-bits reader 3))
        (type (read-bits reader 3)))
    (cond
      ((= type 4)
       (list :version version :type type
             :value (read-literal reader)))
      ((= 0 (read-bits reader 1))
       (list :version version :type type
             :subpackets (read-subpackets-to-length
                           reader (read-bits reader 15))))
      (t
       (list :version version :type type
             :subpackets (loop :repeat (read-bits reader 11)
                               :collect (read-packet reader)))))))

(defun sum-versions (packet)
  (if (= 4 (getf packet :type))
      (getf packet :version)
      (+ (getf packet :version)
         (loop :for subpacket :in (getf packet :subpackets)
               :sum (sum-versions subpacket)))))

(defun solve-part-1 ()
  (sum-versions (read-packet (reader))))

(defun packet-type (packet)
  (getf packet :type))

(defun subpackets (packet)
  (getf packet :subpackets))

(defun eval-packet (packet)
  (eswitch (packet :key #'packet-type)
    (0 (reduce #'+ (mapcar #'eval-packet (subpackets packet))))
    (1 (reduce #'* (mapcar #'eval-packet (subpackets packet))))
    (2 (reduce #'min (mapcar #'eval-packet (subpackets packet))))
    (3 (reduce #'max (mapcar #'eval-packet (subpackets packet))))
    (4 (getf packet :value))
    (5 (if (> (eval-packet (first (subpackets packet)))
              (eval-packet (second (subpackets packet))))
           1 0))
    (6 (if (< (eval-packet (first (subpackets packet)))
              (eval-packet (second (subpackets packet))))
           1 0))
    (7 (if (= (eval-packet (first (subpackets packet)))
              (eval-packet (second (subpackets packet))))
           1 0))))

(defun solve-part-2 ()
  (eval-packet (read-packet (reader))))


