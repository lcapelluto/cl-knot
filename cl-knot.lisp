;;;; cl-knot.lisp
;;;;
;;;; Author: Lauren Capelluto

(in-package #:cl-knot)

;; The game knot, with the minimal number of beadies.
(defparameter *simple-knot* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Beadies ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass beadie ()
  ((id :reader id
       :documentation "A unique constant identifier.")
   (earth :initarg :earth
          :accessor earth
          :documentation "")
   (wind :initarg :wind
          :accessor wind
          :documentation "")
   (fire :initarg :fire
         :accessor fire
         :documentation "")))

(defmethod initialize-instance :after ((object beadie) &key)
  (setf (slot-value object 'id)
        (list (earth object) (wind object) (fire object))))

(defmethod print-object ((object beadie) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~D:  ~D ~D ~D" (id object)
            (earth object)
            (wind object)
            (fire object))))

(defun id-index-difference (index beadie0 beadie1)
  "Return the difference of the INDEX of ID between the beadies."
  (abs (- (nth index (id beadie0)) (nth index (id beadie1)))))

(defun id-difference (beadie0 beadie1)
  "Return the absolute difference of the beadie IDs, per index."
  (loop :for index :in '(0 1 2)
        :collect (id-index-difference index beadie0 beadie1)))

(defun threadp (beadie0 beadie1)
  "Is there a thread between the two beadies?"
  (let ((differences (apply '+ (id-difference beadie0 beadie1))))
    (<= differences 1)))

(defun qualities (beadie)
  "Return the qualities of the beadie as a list."
  (list (earth beadie) (wind beadie) (fire beadie)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Knots ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-simple-knot ()
  "Make a list of beadies with the simplest collections of qualities."
  (let ((curr (list 0 0 0)))
    (loop
      :collect (make-instance 'beadie :earth (first curr)
                                      :wind (second curr)
                                      :fire (third curr))
      :do (setf curr (update-simple-qualities curr))
      :until (null curr))))

(defun update-simple-qualities (qualities)
  "Return the next logical quality trio in the simple knot."
  (loop :do
    (setf qualities (update-quality-index qualities 2))
        :until (or (null qualities) (simple-beadie-p qualities)))
  qualities)

(defun update-quality-index (qualities index)
  "Increase, like in a ternary string, the list of qualities, starting at INDEX and
  moving to the next significant index if needed."
  (let ((new-val (+ 1 (nth index qualities))))
    (cond
      ((< new-val 3)
       (setf (nth index qualities) new-val))
      ((= index 0)
       ;; We've reached the last beadie
       (setf qualities nil))
      (t
       ;; We have to "carry the 1"
       (setf (nth index qualities) 0)
       (setf qualities (update-quality-index qualities (- index 1)))))
    qualities))

(defun simple-beadie-p (qualities)
  "Does this collection of qualities belong in the simple knot?"
  (<= (count 1 qualities) 1))

(defun tangle-knot (knot)
  "Randomly tangle the knot."
  (let
      ((sections (list 'earth 'wind 'fire))
       (directions (list 'pull 'push))
       (planes (list 0 2)))
    (dotimes (i 50)
      (apply-move knot
                  (nth (random 3) sections)
                  (nth (random 2) directions)
                  (nth (random 2) planes))))
  knot)

(defun untangledp (knot)
  "Is the knot untangled (i.e. in the solution state)?"
  (loop :for beadie :in knot
        :when (not (equal (id beadie) (list (earth beadie) (wind beadie) (fire beadie))))
          :do (return nil)
        :finally (return t)))

(defun print-knot (knot stream)
  (loop :for beadie :in knot :do
    (format stream "~a~%" beadie)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Gameplay ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro make-section-movement (name section2 section3)
  "Define the transformation for the given sections."
  `(defun ,name (beadie direction)
     (let ((section2* (,section2 beadie))
           (section3* (,section3 beadie)))
       (cond ((equal direction 'pull)
              (setf (,section2 beadie) (+ 1 (- 1 section3*)))
              (setf (,section3 beadie) (+ 1 (- section2* 1))))
             ((equal direction 'push)
              (setf (,section2 beadie) (+ 1 (- section3* 1)))
              (setf (,section3 beadie) (+ 1 (- 1 section2*))))
             (t
              (error 'unknown direction :option direction))))))

(make-section-movement move-beadie-earth wind fire)
(make-section-movement move-beadie-wind fire earth)
(make-section-movement move-beadie-fire wind earth)

(defun move-beadie (beadie section direction)
  "Modify a single beadie's location as part of a move."
  (cond ((equal section 'earth)
         (move-beadie-earth beadie direction))
        ((equal section 'wind)
         (move-beadie-wind beadie direction))
        ((equal section 'fire)
         (move-beadie-fire beadie direction))))

(defun apply-move (knot section direction plane)
  "Act a complete move upon the knot structure."
  (loop :for beadie :in knot
        :when (= plane (funcall section beadie))
          :do (move-beadie beadie section direction)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Graphics ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(clim:define-application-frame pusheens-home ()
  ((knot :initarg :knot
         :accessor knot))
  (:pointer-documentation t)
  (:panes
   (app :application
        :height 400
        :width 600
        :display-function 'display-pusheens-home)
   (int :interactor
        :height 150
        :width 600))
  (:layouts
   (default (clim:vertically ()
              app int))))

(defun display-pusheens-home (frame pane)
  "How to display Pusheen's Home."
  (print-knot (knot frame) pane)
  (when (untangledp (knot clim:*application-frame*))
    (format pane "Success!")))

;;; Commands and allowed moves

(define-pusheens-home-command (com-quit :menu t) ()
  (clim:frame-exit clim:*application-frame*))

(define-pusheens-home-command (com-play :menu t) ()
  (tangle-knot (knot clim:*application-frame*)))

(defmacro register-move (command section direction plane)
  "Register a new gameplay move."
  (progn
    `(define-pusheens-home-command (,command :name t) ()
       (apply-move (knot clim:*application-frame*) ,section ,direction ,plane))))

;;; Direction mappings swap from L to R, F to B, and D to U so that the transformations
;;; can be independent of the plane.

(register-move com-F 'earth 'pull 0)
(register-move com-Finv 'earth 'push 0)
(register-move com-B 'earth 'push 2)
(register-move com-Binv 'earth 'pull 2)
(register-move com-R 'wind 'pull 0)
(register-move com-Rinv 'wind 'push 0)
(register-move com-L 'wind 'push 2)
(register-move com-Linv 'wind 'pull 2)
(register-move com-D 'fire 'pull 0)
(register-move com-Dinv 'fire 'push 0)
(register-move com-U 'fire 'pull 2)
(register-move com-Uinv 'fire 'push 2)

(defun main (argv)
  "Help untangle Pusheen from her yarn!"
  (declare (ignore argv))
  (setf *simple-knot* (tangle-knot (make-simple-knot)))
  (let ((frame (clim:make-application-frame 'pusheens-home :knot *simple-knot*)))
    (clim:run-frame-top-level frame)))
