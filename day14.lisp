(defpackage #:advent2021.day14
  (:use #:cl #:alexandria #:cl-ppcre #:advent2021.util)
  (:export #:solve-part-1 #:solve-part-2))

(in-package #:advent2021.day14)



(with-puzzle-file (stream)
  (defparameter +template+
    (read-line stream))
  (read-line stream)
  (defparameter +rules+
    (loop :with table := (make-hash-table :test 'equal)
          :for line := (read-line stream nil)
          :while line
          :for (pair result) := (split " -> " line)
          :for (a b) := (coerce pair 'list)
          :do (setf (gethash (cons a b) table) (char result 0))
          :finally (return table))))

(defparameter +input+
  (parse-lines #'identity))

(defun insert-pairs (template)
  (with-output-to-string (out)
    (loop :for (a b) :on (coerce template 'list)
          :while b
          :do (write-char a out)
              (write-char (gethash (cons a b) +rules+) out)
          :finally (write-char a out))))

(defun count-elements (template)
  (loop :with dict := (make-hash-table)
        :for elem :across template
        :do (incf (gethash elem dict 0))
        :finally (return dict)))

(defun freq-difference (dict)
  (loop :for elem :being :the :hash-keys :of dict
        :using (hash-value count)
        :maximize count :into highest
        :minimize count :into lowest
        :finally (return (- highest lowest))))

(defun solve-part-1 ()
  (loop :with template := +template+
        :repeat 10
        :do (setf template (insert-pairs template))
        :finally (return (freq-difference (count-elements template)))))

(defun template-table (template)
  (loop :with table := (make-hash-table :test 'equal)
        :for (a b) :on (coerce template 'list)
        :while b
        :do (incf (gethash (cons a b) table 0))
        :finally (return table)))

(defun insert-pairs* (template-table)
  (loop :with next-generation := (make-hash-table :test 'equal)
        :for (a . b) :being :the :hash-keys :of template-table
        :using (hash-value count)
        :for c := (gethash (cons a b) +rules+)
        :do (incf (gethash (cons a c) next-generation 0) count)
            (incf (gethash (cons c b) next-generation 0) count)
        :finally (return next-generation)))

(defun count-elements* (template-table)
  (loop :with freq-dict := (make-hash-table)
        :with template := (coerce +template+ 'list)
        :for (a . b) :being :the :hash-keys :of template-table
        :using (hash-value count)
        :do (incf (gethash a freq-dict 0) (/ count 2))
            (incf (gethash b freq-dict 0) (/ count 2))
        :finally (progn
                   (incf (gethash (first template) freq-dict) 1/2)
                   (incf (gethash (car (last template)) freq-dict) 1/2)
                   (return freq-dict))))

(defun solve-part-2 ()
  (loop :with template := (template-table +template+)
        :repeat 40
        :do (setf template (insert-pairs* template))
        :finally (return (freq-difference (count-elements* template)))))
