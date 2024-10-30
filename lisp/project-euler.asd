;;;; project-euler.asd

(asdf:defsystem #:project-euler
  :description "My solutions to Project Euler problems"
  :author "Way Yan <wayyan.win@gmail.com>"
  :license  "MIT License"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-utilities)
  :components ((:file "util")))
