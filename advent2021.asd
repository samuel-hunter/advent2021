;;;; advent2021.asd

(asdf:defsystem #:advent2021
  :description "Advent of Code 2021 submission collection and input text"
  :author "Samuel Hunter"
  :license  "Proprietary"
  :version "0.0.1"
  :depends-on (#:alexandria
               #:arrows
               #:cl-ppcre
               #:memoize
               #:queues.priority-queue
               #:split-sequence)
  :serial t
  :components ((:file "util")))
