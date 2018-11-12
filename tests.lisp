;;;; tests.lisp
;;;;
;;;; Author: Lauren Capelluto

(fiasco:define-test-package #:cl-knot-tests
  (:use #:cl-knot)
  (:export #:run-cl-knot-tests))

(in-package #:cl-knot-tests)

(defun run-cl-knot-tests ()
  "Run all tests."
  (setf fiasco::*test-run-standard-output* (make-broadcast-stream
                                            *standard-output*))
  (run-package-tests :package ':cl-knot-tests
                     :describe-failures t
                     :interactive t))

(defparameter *b0* (make-instance 'cl-knot::beadie :earth 0 :wind 1 :fire 2))
(defparameter *b1* (make-instance 'cl-knot::beadie :earth 0 :wind 2 :fire 2))
(defparameter *b2* (make-instance 'cl-knot::beadie :earth 1 :wind 2 :fire 2))

(deftest test-simple-beadie-p ()
  (is (cl-knot::simple-beadie-p (cl-knot::id *b0*)))
  (is (not (cl-knot::simple-beadie-p (list 1 1 2)))))

(deftest test-untangledp ()
  (is (cl-knot::untangledp (cl-knot::make-simple-knot))))

(deftest test-simple-knot ()
  (is (= 20 (length (cl-knot::make-simple-knot))))
  (is (= 20 (length (cl-knot::make-simple-knot)))))

(deftest test-threadp ()
  (is (cl-knot::threadp *b0* *b1*))
  (is (cl-knot::threadp *b1* *b2*))
  (is (cl-knot::threadp *b2* *b1*))
  (is (not (cl-knot::threadp *b0* *b2*))))
