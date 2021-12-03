(defpackage #:advent2021.day03
  (:use #:cl #:alexandria #:split-sequence #:advent2021.util)
  (:export #:solve-part-1 #:solve-part-2))

(in-package #:advent2021.day03)



(defparameter +input+
  (parse-lines
    (lambda (line)
      (loop :for c :across line
            :collect (coerce (parse-integer (string c)) 'bit) :into bits
            :finally (return (make-array (length line)
                                         :element-type 'bit
                                         :initial-contents bits))))))

(defparameter +bit-size+ 12)

(defun bitmap->integer (bitmap)
  (loop :for i :below (length bitmap)
        :with n := 0
        :do (setf n (+ (bit bitmap i)
                       (ash n 1)))
        :finally (Return n)))

(defun count-bit (bit-maps biti)
  (loop :for bit-map :in bit-maps
        :count (zerop (bit bit-map biti)) :into zeroes
        :count (= 1 (bit bit-map biti)) :into ones
        :finally (return (if (> ones zeroes) 1 0))))

(defun gamma-rate ()
  (loop :for biti :below +bit-size+
        :collect (count-bit +input+ biti) :into bits
        :finally (Return (make-array +bit-size+ :element-type 'bit
                                     :initial-contents bits))))

(defun gamma ()
  (bitmap->integer (gamma-rate)))

(defun epsilon ()
  (bitmap->integer (bit-not (gamma-rate))))

(defun solve-part-1 ()
  (* (gamma) (epsilon)))

(defun filter-bitmaps (bitmaps criteria)
  (loop :while (> (length bitmaps) 1)
        :for biti :upfrom 0
        :do (setf bitmaps (remove-if-not (rcurry criteria biti bitmaps) bitmaps))
        :finally (Return (first bitmaps))
        ))

(defun most-common-bit (bit-maps biti)
  (loop :for bit-map :in bit-maps
        :count (zerop (bit bit-map biti)) :into zeroes
        :count (= 1 (bit bit-map biti)) :into ones
        :finally (return (cond
                           ((> ones zeroes) 1)
                           ((> zeroes ones) 0)))))

(defun oxy-gen-test (bm biti cur-maps)
  (= (bit bm biti) (or (most-common-bit cur-maps biti) 1))
  )

(defun oxy-generator ()
  (bitmap->integer (filter-bitmaps +input+ #'oxy-gen-test)))

(defun test-co2 (bm biti cur-maps)
  (= (bit bm biti) (if (eql (most-common-bit cur-maps biti) 0) 1 0)))

(defun co2 ()
  (bitmap->integer (filter-bitmaps +input+ #'test-co2)))

(defun solve-part-2 ()
  (* (oxy-generator) (co2)))
