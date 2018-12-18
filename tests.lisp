;;;; tests.lisp
;;;;
;;;; Author: Lauren Capelluto

(fiasco:define-test-package #:cl-knot-tests
  (:use #:cl
        #:fiasco
        #:cl-knot)
  (:export #:run-cl-knot-tests))

(in-package #:cl-knot-tests)

(defun run-cl-knot-tests ()
  "Run all tests."
  (setf fiasco::*test-run-standard-output* (make-broadcast-stream *standard-output*))
  (fiasco:run-package-tests :package '#:cl-knot-tests
                            :describe-failures t
                            :verbose t
                            :interactive nil))

(defparameter *b012* (cl-knot::make-beadie 0 1 2))
(defparameter *b221* (cl-knot::make-beadie 0 2 2))
(defparameter *b122* (cl-knot::make-beadie 1 2 2))
(defparameter *knot* (make-instance 'cl-knot::simple-knot))

(deftest test-simple-beadie-p ()
  "Test that we can verify whether a beadie belongs in the simple knot."
  (is (cl-knot::simple-beadie-p (cl-knot::solved-pos *b012*)))
  (is (not (cl-knot::simple-beadie-p (list 1 1 2)))))

(deftest test-untangledp ()
  "Test that we know if the knot is untangled."
  (is (cl-knot::untangledp (make-instance 'cl-knot::simple-knot))))

(deftest test-simple-knot ()
  "Test that the simple knot has the correct number of beadies, and can be made twice."
  (is (= 20 (length (cl-knot::make-simple-knot))))
  (is (= 20 (length (cl-knot::make-simple-knot)))))

(deftest test-threadp ()
  "Test that we know which beadies are adjacent."
  (is (cl-knot::threadp *knot* *b012* *b221*))
  (is (cl-knot::threadp *knot* *b221* *b122*))
  (is (cl-knot::threadp *knot* *b122* *b221*))
  (is (not (cl-knot::threadp *knot* *b012* *b122*))))

(deftest test-inverse-moves ()
  "Test that each move is undone by its inverse."
  (let ((L (cl-knot::make-plane 'x 0))
        (D (cl-knot::make-plane 'y 0))
        (F (cl-knot::make-plane 'z 0))
        (B (cl-knot::make-plane 'z 2)))
    (is (cl-knot::untangledp *knot*))
    (cl-knot::apply-move *knot* L 'cl-knot::pull)
    (cl-knot::apply-move *knot* L 'cl-knot::push)
    (is (cl-knot::untangledp *knot*))
    (cl-knot::apply-move *knot* D 'cl-knot::push)
    (cl-knot::apply-move *knot* D 'cl-knot::pull)
    (is (cl-knot::untangledp *knot*))
    (cl-knot::apply-move *knot* F 'cl-knot::push)
    (cl-knot::apply-move *knot* F 'cl-knot::pull)
    (is (cl-knot::untangledp *knot*))
    (cl-knot::apply-move *knot* B 'cl-knot::pull)
    (cl-knot::apply-move *knot* B 'cl-knot::push)))

(deftest test-move-transformation ()
  "Test that the transformations created by the moves are as expected."
  (labels ((qualities (b)
             (list (cl-knot::x-pos b)
                   (cl-knot::y-pos b)
                   (cl-knot::z-pos b))))
    (let ((b000 (cl-knot::make-beadie 0 0 0)))
      (cl-knot::rotate-x *knot* b000 'cl-knot::pull)
      (is (equal (list 0 2 0) (qualities b000)))
      (cl-knot::rotate-y *knot* b000 'cl-knot::pull)
      (is (equal (list 0 2 2) (qualities b000)))
      (cl-knot::rotate-z *knot* b000 'cl-knot::pull)
      (is (equal (list 2 2 2) (qualities b000))))
    (let ((b122 (cl-knot::make-beadie 1 2 2)))
      ;; Why not?
      (cl-knot::rotate-y *knot* b122 'cl-knot::push)
      (is (equal (list 0 2 1) (qualities b122)))
      (cl-knot::rotate-x *knot* b122 'cl-knot::push)
      (is (equal (list 0 1 0) (qualities b122)))
      ;; Rotate ze knot!
      (cl-knot::rotate-z *knot* b122 'cl-knot::push)
      (is (equal (list 1 0 0) (qualities b122))))))
