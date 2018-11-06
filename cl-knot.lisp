;;;; cl-knot.lisp
;;;;
;;;; Author: Lauren Capelluto

;; update README

(in-package #:cl-knot)

(defparameter *simple-knot* nil)
;(defparameter *moves* (list +F+ +B+ +L+ +R+ +U+ +D+))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; making beads                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass bead ()
  ((id :reader id
       :documentation "")
   (earth :initarg :earth
          :accessor earth
          :documentation "")
   (wind :initarg :wind
          :accessor wind
          :documentation "")
   (fire :initarg :fire
         :accessor fire
         :documentation "")))

(defmethod initialize-instance :after ((beadie bead) &key)
  (setf (slot-value beadie 'id)
        (list (earth beadie) (wind beadie) (fire beadie))))

(defun diff (feature b0 b1)
  (abs (- (funcall feature b0) (funcall feature b1))))

; why isn't this an error?
(defmethod compare (b0 b1)
  (make-instance 'bead :earth (diff 'earth b0 b1)
                       :wind (diff 'wind b0 b1)
                       :fire (diff 'fire b0 b1)))

(defun has-thread (beadie0 beadie1)
  ;; TODO ID based
  (let ((differences 0)
        (sum-beadies (compare beadie0 beadie1)))
    (loop :for feature in '(earth wind fire) :do
      (setq differences (+ differences (funcall feature sum-beadies))))
    (<= differences 1)))

(defmethod print-object ((beadie bead) stream)
  (print-unreadable-object (beadie stream :type t)
    (format stream "~D:  ~D ~D ~D" (id beadie)
            (earth beadie)
            (wind beadie)
            (fire beadie))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; making knots                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun valid-simple-bead (qualities)
   ; TODO check type
  (if (null qualities)
      nil
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
                                     :fire (caddr qualities))))

(defun create-knot ()
  ;; TODO replace all-simple-bead-qualities
  (loop :for qualities :in (all-simple-bead-qualities)
        :for magic :from 0
        collect (make-instance 'bead :earth (car qualities)
                                     :wind (cadr qualities)
                                     :fire (caddr qualities))))

(defun print-knot (knot stream)
  (loop :for beadie :in knot :do
    (format stream "~a~%" beadie)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gameplay                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun untangled-p (knot)
  (loop :for beadie :in knot
        when (not (equal (id beadie) (list (earth beadie) (wind beadie) (fire beadie))))
          do (return nil)
        finally (return t)))

(defun move-beadie (beadie section direction)
  (declare (ignore direction section))
  ;; point = earth, wind, fire, remove third by section
  ;; x = x - 1, y = y - 1
  ;; x -> -y, y -> x (times overall -1 depending on direction)
  ;; x += 1, y += 1           counter (pull)   clockwise (push)
  ;; x = +/-(-y + 1) + 1 >>>> x = 2 - y        or x = y
  ;; y = +/-(x - 1) + 1  >>>> y = x            or y = 2 - x
  (let ((wind* (wind beadie))
        (fire* (fire beadie)))
    ;; pull
    (setf (wind beadie) (+ 1 (- 1 fire*)))
    (setf (fire beadie) (+ 1 (- wind* 1)))))

;; TODO: macro for moves?
(defun make-move (knot section direction)
  ; x = 0 plane, clockwise rotation - F
  ; lets call direction "up" or "down" instead of rotational
  ; first 8 beadies end up in new place
  (loop :for beadie :in knot
        ; TODO by section, a quality name and a number 0 or 2
        when (= 2 (earth beadie))
          do (move-beadie beadie section direction)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; graphics                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defclass pusheen-home-pane
;;     (clim:clim-stream-pane) ())

;; (defmethod handle-repaint ((pane pusheen-home-pane) region)
;;   (let ((w (bounding-rectangle-width pane))
;;         (h (bounding-rectangle-height pane)))
;;     (draw-rectangle* pane 0 0 w h
;;                      :filled t
;;                      :ink (pane-background pane))
;;     (draw-text* pane
;;                 (greeting *application-frame*)
;;                 (floor w 2) (floor h 2)
;;                 :align-x :center
;;                 :align-y :center)))

(define-application-frame pusheens-home ()
  ((greeting :initform "Hello World"
             :accessor greeting)
   (knot :initarg :knot
         :accessor knot))
  (:pointer-documentation t)
  (:panes
   ; TODO rename
   (app :application
        :height 400
        :width 600
        :display-function 'display-pusheens-home)
   (int :interactor
        :height 400
        :width 600))
  (:layouts
   (default (horizontally ()
              app int))))

(defun display-pusheens-home (frame pane)
  (print-knot (knot frame) pane))

(define-pusheens-home-command (com-quit :menu t) ()
  (frame-exit *application-frame*))

(define-pusheens-home-command (com-parity :name t) ((number 'integer))
  (setf (current-number *application-frame*) number))

(defun %main (argv)
  "Help Pusheen untangle her knotted yarn!"
  (declare (ignore argv))

  (setf *simple-knot* (create-simple-knot))
  ;; TODO replace with random moves to knot it up
  (make-move *simple-knot* 'earth 'pull)
  (make-move *simple-knot* 'earth 'pull)
  (let ((frame (make-application-frame 'pusheens-home :knot *simple-knot*)))
    (run-frame-top-level frame))

  ;; accept moves
  ;; check if untangled after each move (untangled-p *simple-knot*)
  nil)
