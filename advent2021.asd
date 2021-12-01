;;;; advent2021.asd

(asdf:defsystem #:advent2021
  :description "Describe advent2021 here"
  :author "Samuel Hunter"
  :license  "Specify license here"
  :version "0.0.1"
  :depends-on (#:alexandria
               #:arrows
               #:cl-ppcre
               #:memoize
               #:split-sequence)
  :serial t
  :components ((:file "util")))
