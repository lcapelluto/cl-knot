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
                     :verbose t
                     :interactive nil))

(defparameter *b012* (make-instance 'cl-knot::beadie :earth 0 :wind 1 :fire 2))
(defparameter *b221* (make-instance 'cl-knot::beadie :earth 0 :wind 2 :fire 2))
(defparameter *b122* (make-instance 'cl-knot::beadie :earth 1 :wind 2 :fire 2))

(deftest test-simple-beadie-p ()
  "Test that we can verify whether a beadie belongs in the simple knot."
  (is (cl-knot::simple-beadie-p (cl-knot::id *b012*)))
  (is (not (cl-knot::simple-beadie-p (list 1 1 2)))))

(deftest test-untangledp ()
  "Test that we know if the knot is untangled."
  (is (cl-knot::untangledp (cl-knot::make-simple-knot))))

(deftest test-simple-knot ()
  "Test that the simple knot has the correct number of beadies, and can be made twice."
  (is (= 20 (length (cl-knot::make-simple-knot))))
  (is (= 20 (length (cl-knot::make-simple-knot)))))

(deftest test-threadp ()
  "Test that we know which beadies are adjacent."
  (is (cl-knot::threadp *b012* *b221*))
  (is (cl-knot::threadp *b221* *b122*))
  (is (cl-knot::threadp *b122* *b221*))
  (is (not (cl-knot::threadp *b012* *b122*))))

(defparameter *knot* (cl-knot::make-simple-knot))

(deftest test-inverse-moves ()
  "Test that each move is undone by its inverse."
  (is (cl-knot::untangledp *knot*))
  (cl-knot::apply-move *knot* 'cl-knot::earth 'cl-knot::pull 0)
  (cl-knot::apply-move *knot* 'cl-knot::earth 'cl-knot::push 0)
  (is (cl-knot::untangledp *knot*))
  (cl-knot::apply-move *knot* 'cl-knot::wind 'cl-knot::push 0)
  (cl-knot::apply-move *knot* 'cl-knot::wind 'cl-knot::pull 0)
  (is (cl-knot::untangledp *knot*))
  (cl-knot::apply-move *knot* 'cl-knot::fire 'cl-knot::push 0)
  (cl-knot::apply-move *knot* 'cl-knot::fire 'cl-knot::pull 0)
  (is (cl-knot::untangledp *knot*))
  (cl-knot::apply-move *knot* 'cl-knot::wind 'cl-knot::pull 0)
  (cl-knot::apply-move *knot* 'cl-knot::wind 'cl-knot::push 0)
  (is (cl-knot::untangledp *knot*))
  (cl-knot::apply-move *knot* 'cl-knot::fire 'cl-knot::pull 2)
  (cl-knot::apply-move *knot* 'cl-knot::fire 'cl-knot::push 2))

(deftest test-move-transformation ()
  "Test that the transformations created by the moves are as expected."
  (let ((b000 (make-instance 'cl-knot::beadie :earth 0 :wind 0 :fire 0)))
    (cl-knot::move-beadie b000 'cl-knot::earth 'cl-knot::pull)
    (is (equal (list 0 2 0) (cl-knot::qualities b000)))
    (cl-knot::move-beadie b000 'cl-knot::wind 'cl-knot::pull)
    (is (equal (list 0 2 2) (cl-knot::qualities b000)))
    (cl-knot::move-beadie b000 'cl-knot::fire 'cl-knot::pull)
    (is (equal (list 2 2 2) (cl-knot::qualities b000))))
  (let ((b122 (make-instance 'cl-knot::beadie :earth 1 :wind 2 :fire 2)))
    (cl-knot::move-beadie b122 'cl-knot::wind 'cl-knot::push)
    (is (equal (list 0 2 1) (cl-knot::qualities b122)))
    (cl-knot::move-beadie b122 'cl-knot::earth 'cl-knot::push)
    (is (equal (list 0 1 0) (cl-knot::qualities b122)))
    (cl-knot::move-beadie b122 'cl-knot::fire 'cl-knot::push)
    (is (equal (list 1 0 0) (cl-knot::qualities b122)))))
