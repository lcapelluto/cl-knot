;;;; cl-knot-tests.asd
;;;;
;;;; Author: Lauren Capelluto

(asdf:defsystem #:cl-knot-tests
  :description "Tests for CL-KNOT."
  :author "Lauren Capelluto"
  :depends-on (#:cl-knot
               #:fiasco)
  :components ((:file "tests")))
