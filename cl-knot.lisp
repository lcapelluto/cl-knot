;;;; cl-knot.lisp
;;;;
;;;; Author: Lauren Capelluto

;; TODO need to fix bug where (all-simple-beads) thinks it should start at '(2 0 0)
;; TODO style and cleaning (reorganizing)
;; TODO renaming for thematical consistency
;; TODO GUI.... T-T
;; TODO main
;; TODO general knot, maybe other knots
;; define allowed moves
;; define transformations for each move which change the knot
;; try knoting and unknoting
;; meta: simplify syntax with macros hopefully
;; write tests
;; documentation strings
;; make beadies more printable

(in-package #:cl-knot)

(defparameter *simple-knot* (create-simple-knot))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; making beads                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; enum? LoL esk? characters?
(defclass bead ()
  ((id :initarg :id
             :reader id
             :documentation "")
   (earth :initarg :earth
          :reader earth
          :documentation "")
   (wind :initarg :wind
          :reader wind
          :documentation "")
   (fire :initarg :fire
         :reader fire
         :documentation ""))
  (:default-initargs :earth 0
                     :wind 0
                     :fire 0))

; TODO make these methods instead
(defun diff (feature b0 b1)
  (abs (- (funcall feature b0) (funcall feature b1))))

(defmethod compare (b0 b1)
  (make-instance 'bead :earth (diff 'earth b0 b1)
                       :wind (diff 'wind b0 b1)
                       :fire (diff 'fire b0 b1)))

(defun has-thread (beadie0 beadie1)
  (let ((differences 0)
        (sum-beadies (compare beadie0 beadie1)))
    (loop :for feature in '(earth wind fire) :do
      (setq differences (+ differences (funcall feature sum-beadies))))
    (<= differences 1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; making knots                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun valid-simple-bead (qualities)
  ; TODO check type
  (if (null qualities)
      'nil
      (let ((num-ones 0))
        (loop for q :in qualities :do
          (if (= q 1)
              (setf num-ones (+ 1 num-ones))))
        (<= num-ones 1))))

(defun update-quality-index (qualities index)
  (let ((new-val (+ 1 (nth index qualities))))
    (cond ((< new-val 3)
           (setf (nth index qualities) new-val)
           qualities)
          ((= index 0)
           ;; We've reached the last bead
           (setf qualities nil)
           qualities)
          (t
           (setf (nth index qualities) 0)
           (setf qualities (update-quality-index qualities (- index 1)))
           qualities))))

(defun update-simple-qualities (qualities)
  "Return the next logical collection of qualities in the simple knot."
  ; TODO (check-type next three-tuple)
  (loop :do
    (setf qualities (update-quality-index qualities 2))
        :until (or (null qualities) (valid-simple-bead qualities)))
  qualities)

(defun all-simple-bead-qualities ()
  (let ((curr '(0 0 0))
         (quality-list nil))
    (loop
      (setf quality-list (append quality-list (list (copy-list curr))))
      (setf curr (update-simple-qualities curr))
      (when (null curr) (return)))
    quality-list))

(defun create-simple-knot ()
  (loop :for qualities :in (all-simple-bead-qualities)
        :for magic :from 0
        collect (make-instance 'bead :earth (car qualities)
                                     :wind (cadr qualities)
                                     :fire (caddr qualities)
                                     :id qualities)))

(defun create-knot ()
  ;; TODO replace all-simple-bead-qualities
  (loop :for qualities :in (all-simple-bead-qualities)
        :for magic :from 0
        collect (make-instance 'bead :earth (car qualities)
                                     :wind (cadr qualities)
                                     :fire (caddr qualities)
                                     :id qualities)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gameplay                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun untangled-p (knot)
  (loop :for beadie :in knot
        :for magic :from 0
        when (not (= (sym beadie) magic))
          do (return nil)
        finally (return t)))

;; TODO: macro for moves?
(defun make-move (knot section direction)
  ; x = 0 plane, clockwise rotation - F
  ; lets call direction "up" or "down" instead of rotational
  ; first 8 beadies end up in new place
  )

(defun %main (argv)
  "Help Pusheen untangle her knotted yarn!"
  (declare (ignore argv))
  ;; TODO make random moves to knot it up
  ;; start gui
  ;; accept moves
  ;; check if untangled after each move
  nil)
