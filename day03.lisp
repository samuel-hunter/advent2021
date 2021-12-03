(defpackage #:advent2021.day03
  (:use #:cl #:alexandria #:split-sequence #:advent2021.util)
  (:export #:solve-part-1 #:solve-part-2))

(in-package #:advent2021.day03)



;; Parse input into a list of simple bit-arrays.
(defparameter +input+
  (parse-lines
    (lambda (line)
      (loop :with bit-map := (make-array (length line)
                                         :element-type 'bit)
            :for c :across line
            :for biti :upfrom 0
            :do (setf (sbit bit-map biti)
                      (if (char= c #\1) 1 0))
            :finally (return bit-map)))))

(defun push-bit (integer b)
  "Shift integer by 1 bit and increment by b."
  (+ (ash integer 1) b))

(define-modify-macro pushf-bit (b) push-bit
                     "Shift place by 1 bit and increment by b.")

(defun bitmap->integer (bitmap)
  (loop :for i :below (length bitmap)
        :with n := 0
        :do (pushf-bit n (sbit bitmap i))
        :finally (return n)))

(defun most-common-bit (bit-arrays biti)
  (loop :for bit-array :in bit-arrays
        :for zero? := (zerop (sbit bit-array biti))
        :count zero? :into zeroes
        :count (not zero?) :into ones
        :finally (return (if (>= ones zeroes) 1 0))))

(defun median-bit-array (bit-arrays)
  "Return a bit-array with the most common bit of each place."
  (loop :with bits := (length (first bit-arrays))
        :with result := (make-array bits :element-type 'bit)
        :for biti :below bits
        :do (setf (sbit result biti)
                  (most-common-bit bit-arrays biti))
        :finally (return result)))

(defun gamma-rate ()
  (bitmap->integer (median-bit-array +input+)))

(defun epsilon-rate ()
  (bitmap->integer (bit-not (median-bit-array +input+))))

(defun solve-part-1 ()
  (* (gamma-rate) (epsilon-rate)))

(defun sieve-bit-arrays (bit-arrays criteria)
  (loop :while (> (length bit-arrays) 1)
        :for biti :upfrom 0
        :for most-common-bit := (most-common-bit bit-arrays biti)
        :do (setf bit-arrays
                  (remove-if-not (rcurry criteria most-common-bit) bit-arrays
                                 :key (rcurry #'sbit biti)))
        :finally (return (first bit-arrays))))

(defun o2-gen-rating ()
  (bitmap->integer (sieve-bit-arrays +input+ #'=)))

(defun co2-scrubber-rating ()
  (bitmap->integer (sieve-bit-arrays +input+ (complement #'=))))

(defun solve-part-2 ()
  (* (o2-gen-rating) (co2-scrubber-rating)))
