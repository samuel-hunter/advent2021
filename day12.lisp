(defpackage #:advent2021.day12
  (:use #:cl #:alexandria #:cl-ppcre #:advent2021.util)
  (:export #:solve-part-1 #:solve-part-2))

(in-package #:advent2021.day12)



(defparameter +input+
  (loop :with map := (make-hash-table)
        :for line :in (parse-lines #'identity)
        :for (from to) := (mapcar (lambda (kw)
                                    (intern kw :keyword))
                                  (split "-" line))
        :do (push to (gethash from map))
            (push from (gethash to map))
        :finally (return map)))

(defun possible-paths (start &optional route)
  (cond
    ((eq start :|end|) 1)
    ((member start route) 0)
    (t
     (loop :for next-node :in (gethash start +input+)
           :sum (possible-paths next-node
                                (if (upper-case-p
                                      (char (string start) 0))
                                    route
                                    (cons start route)))))))

(defun solve-part-1 ()
  (possible-paths :|start|))

(defun large? (node)
  (upper-case-p (char (string node) 0)))

(defun cave-visited-twice? (route)
  (loop :with small-caves := ()
        :for cave :in route
        :do (when (not (large? cave))
              (if (member cave small-caves)
                  (return t)
                  (push cave small-caves)))))

(defun should-visit? (node route)
  (when (eq node :|start|)
      (return-from should-visit? nil))

  (or (large? node)
      (< (loop :for n :in route
            :count (eq n node))
         (if (cave-visited-twice? route)
             1 2))))

(defun possible-paths* (start &optional route)
  (cond
    ((eq start :|end|)
     ;; (print (list :path route))
     1)
    (t (loop :for next-node :in (gethash start +input+)
             :when (should-visit? next-node (cons start route))
             :sum (possible-paths* next-node (cons start route))))))

(defun solve-part-2 ()
  (possible-paths* :|start|))
