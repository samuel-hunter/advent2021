(defpackage #:advent2021.day08
  (:use #:cl #:alexandria #:cl-ppcre #:advent2021.util)
  (:export #:solve-part-1 #:solve-part-2))

(in-package #:advent2021.day08)



(defparameter +input+
  ;; (signals outputs)
  (parse-lines
    (lambda (line)
      (destructuring-bind (signal output) (split " \\| " line)
        (list (split " " signal)
              (split " " output))))))

(defun solve-part-1 ()
  (loop :for (signals outputs) :in +input+
        :sum (loop :for output :in outputs
                   :count (member (length output) '(3 2 4 7)))))

;;  1111
;; 2    3
;; 2    3
;;  4444
;; 5    6
;; 5    6
;;  7777

(defun find-signal (n signals)
  "Find the signal from SIGNALS with the provided number of segments"
  (loop :for signal :in signals
        :until (and (= n (length signal)))
        :finally (return signal)))

(defun one (signals)
  (find-signal 2 signals))

(defun seven (signals)
  (find-signal 3 signals))

(defun four (signals)
  (find-signal 4 signals))

(defun eight (signals)
  (find-signal 7 signals))

(defun zero (signals one four)
  (loop :with sig3or6 := (char one 0)
        :with sig6or3 := (char one 1)
        :for signal :in signals
        :when (and (= 6 (length signal))
                   (and (position sig3or6 signal)
                        (position sig6or3 signal))
                   (not (loop :for seg :across four
                              :always (position seg signal))))
        :return signal))

(defun six (signals one)
  (loop :with sig3or6 := (char one 0)
        :with sig6or3 := (char one 1)
        :for signal :in signals
        :when (and (= 6 (length signal))
                   (xor (position sig3or6 signal)
                        (position sig6or3 signal)))
        :return signal))

(defun nine (signals one four)
  (loop :with sig3or6 := (char one 0)
        :with sig6or3 := (char one 1)
        :for signal :in signals
        :when (and (= 6 (length signal))
                   (and (position sig3or6 signal)
                        (position sig6or3 signal))
                   (loop :for seg :across four
                         :always (position seg signal)))
        :return signal))

(defun seg1 (seven one)
  (loop :for seg :across seven
        :while (position seg one)
        :finally (return seg)))

(defun missing-seg (signal &optional (out-of "abcdefg"))
  "Find the segment from SIGNAL that's missing out of OUT-OF."
  (loop :for seg :across out-of
        :while (position seg signal)
        :finally (return seg)))

(defun seg6 (one seg3)
  (loop :for seg :across one
        :while (char= seg seg3)
        :finally (return seg)))

(defun segment-map (signals)
  "Return a string of segments that map to its respective locations."
  (let* ((one (one signals))
         (four (four signals))
         (seven (seven signals))

         (zero (zero signals one four))
         (six (six signals one))
         (nine (nine signals one four))

         (seg1 (seg1 seven one))
         (seg3 (missing-seg six))
         (seg4 (missing-seg zero))
         (seg5 (missing-seg nine))
         (seg6 (seg6 one seg3))

         (seg2 (missing-seg (coerce (list seg3 seg4 seg6) 'string)
                            four))
         (seg7 (missing-seg (coerce (list seg1 seg2 seg3 seg4 seg5 seg6)
                                    'string))))
    (coerce (list seg1 seg2 seg3 seg4 seg5 seg6 seg7)
            'string)))

(defparameter +segment-table+
  '((1 2 3 5 6 7)
    (3 6)
    (1 3 4 5 7)
    (1 3 4 6 7)
    (2 3 4 6)
    (1 2 4 6 7)
    (1 2 4 5 6 7)
    (1 3 6)
    (1 2 3 4 5 6 7)
    (1 2 3 4 6 7))
  "The segment locations that map to each digit.")

(defun output-digit (output segment-map)
  (loop :for seg :across output
        :collect (1+ (position seg segment-map)) :into locs
        :finally (return (position (sort locs #'<)
                                   +segment-table+ :test #'equal))))

(defun output (outputs segment-map)
  (loop :with n := 0
        :for output :in outputs
        :do (setf n (+ (* n 10)
                       (output-nums output segment-map)))
        :finally (return n)))

(defun solve-part-2 ()
  (loop :for (signals outputs) :in +input+
        :for segment-map := (segment-map signals)
        :for output := (output outputs segment-map)
        :sum output))
