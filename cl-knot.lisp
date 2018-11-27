;;;; cl-knot.lisp
;;;;
;;;; Author: Lauren Capelluto

(in-package #:cl-knot)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Beadies ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass beadie ()
  ((id :reader id
       :documentation "A unique constant identifier.")
   (x-position :initarg :x-position
               :accessor x-pos
               :documentation "The beadie location on the x-axis.")
   (y-position :initarg :y-position
               :accessor y-pos
               :documentation "The beadie location on the y-axis.")
   (z-position :initarg :z-position
               :accessor z-pos
               :documentation "The beadie location on the z-axis.")))

(defmacro make-beadie (x y z)
  `(make-instance 'beadie :x-position ,x :y-position ,y :z-position ,z))

(defmethod initialize-instance :after ((object beadie) &key)
  (setf (slot-value object 'id)
        (list (x-pos object) (y-pos object) (z-pos object))))

(defmethod print-object ((object beadie) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~D:  ~D ~D ~D" (id object)
            (x-pos object)
            (y-pos object)
            (z-pos object))))

(defun id-index-difference (index beadie0 beadie1)
  "Return the difference of the INDEX of ID between the beadies."
  (abs (- (nth index (id beadie0)) (nth index (id beadie1)))))

(defun id-difference (beadie0 beadie1)
  "Return the absolute difference of the beadie IDs, per index."
  (dolist (index '(0 1 2))
    :collect (id-index-difference index beadie0 beadie1)))

(defun threadp (beadie0 beadie1)
  "Is there a thread between the two beadies?"
  (let ((differences (apply '+ (id-difference beadie0 beadie1))))
    (<= differences 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Knots ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass knot ()
  ((beadies :accessor knot-beadies
            :documentation "The beadies which are part of the knot.")
   (center :accessor center
           :documentation "Central point to rotate beadies around."))
  (:documentation "The game knot which we attempt to untangle Pusheen from"))

(defclass simple-knot (knot)
  ((beadies :initform (make-simple-knot))
   (center :initform (make-point 1.0 1.0 1.0))))

(defun make-simple-knot ()
  "Make a list of beadies roughly patterned after a void cube."
  (let ((curr (list 0 0 0)))
    (loop
      :collect (make-beadie (first curr) (second curr) (third curr))
      :do (setf curr (update-simple-qualities curr))
      :until (null curr))))

(defun update-simple-qualities (qualities)
  "Return the next logical quality trio in the simple knot."
  (loop :do
    (setf qualities (update-quality-index qualities))
        :until (or (null qualities) (simple-beadie-p qualities)))
  qualities)

(defun update-quality-index (qualities &optional (index 2))
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

(defclass tiny-knot (knot)
  ((beadies :initform (make-tiny-knot))
   (center :initform (make-point 0.5 0.5 0.5))))

(defun make-tiny-knot ()
  "Make a list of just 8 beadies, matching a 2x2x2 cube."
  (let ((knot nil))
    (loop :for x :in (list 0 1) :do
      (loop :for y :in (list 0 1) :do
        (loop :for z :in (list 0 1) :do
          (push (make-beadie x y z) knot))))
    knot))

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
  (loop :for beadie :in (knot-beadies knot)
        :when (not (equal (id beadie)
                          (list (x-pos beadie) (y-pos beadie) (z-pos beadie))))
          :do (return nil)
        :finally (return t)))

(defmethod print-object ((object knot) stream)
  (print-unreadable-object (object stream :type t)
    (dolist (beadie (knot-beadies object))
      (format stream "~a~%" beadie))))

(defstruct (point (:constructor make-point (x y z)))
  "A 3D point."
  (x 0 :type float)
  (y 0 :type float)
  (z 0 :type float))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Gameplay ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro make-axis-rotation (axis)
  "Define a generic function and method specialized on the knot class to rotate a given beadie pi/2 either clockwise or counterclockwise around AXIS. AXIS can be X, Y or Z.

The defined function is called ROTATE-AXIS where AXIS is replaced by its value, and it takes a KNOT instance, BEADIE instance, and DIRECTION (either 'pull or 'push) as input."
  (let* (;; Define the first and second axis such that axis1 curl axis2 is AXIS
         (axis1 (cond ((equal axis 'x) 'y)
                      ((equal axis 'y) 'z)
                      ((equal axis 'z) 'x)))
         (axis2 (cond ((equal axis 'x) 'z)
                      ((equal axis 'y) 'x)
                      ((equal axis 'z) 'y)))
         (func-name (intern (format nil "ROTATE-~a" axis)))
         (axis1-pos (intern (format nil "~a-POS" axis1)))
         (axis2-pos (intern (format nil "~a-POS" axis2))))
    `(defgeneric ,func-name (knot beadie direction)
       (:documentation "TODO")
       (:method ((knot knot) beadie direction)
         (let* ((ax1-center (,(intern (format nil "POINT-~a" axis1)) (center knot)))
                (ax2-center (,(intern (format nil "POINT-~a" axis2)) (center knot)))
                (ax1-beadie-pos (,axis1-pos beadie))
                (ax2-beadie-pos (,axis2-pos beadie))
                (ax1-sign (if (equal direction 'pull)
                                -1
                                1))
                (ax2-sign (* -1 ax1-sign)))
           (setf (,axis1-pos beadie)
                 (+ ax1-center
                    (* ax1-sign
                       (- ax2-beadie-pos ax2-center))))
           (setf (,axis2-pos beadie)
                 (+ ax2-center
                    (* ax2-sign
                       (- ax1-beadie-pos ax1-center)))))))))

(make-axis-rotation x)
(make-axis-rotation y)
(make-axis-rotation z)

(defun move-beadie (knot beadie section direction)
  "Modify a single beadie's location as part of a move."
  (cond ((equal section 'earth)
         (rotate-x knot beadie direction))
        ((equal section 'wind)
         (rotate-y knot beadie direction))
        ((equal section 'fire)
         (rotate-z knot beadie direction))))

(defgeneric apply-move (knot section direction plane)
  (:documentation "Act a complete move upon the knot structure.")

  (:method ((knot knot) section direction plane)
    (dolist (beadie (knot-beadies knot))
      :when (= plane (funcall section beadie))
      :do (move-beadie knot beadie section direction)))

  (:method ((knot tiny-knot) section direction plane)
    ;; The normal dimension is 2, but the tiny knot is, well, tiny.
    (when (= plane 2) (setf plane 1))
    (call-next-method)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Graphics ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(clim:define-application-frame pusheens-home ()
  ((knot :initarg :knot
         :accessor knot))
  (:panes
   (app :application
        :height 400
        :width 600
        :scroll-bars nil
        :display-function 'display-pusheens-home)
   (int :interactor
        :height 150
        :width 600)
   (F   :push-button
        :label "F"
        :activate-callback #'(lambda (x)
                               (declare (ignore x))
                               (format t "Hit the F button."))))
  (:layouts
   (default (clim:vertically ()
              app (clim:horizontally () int
                                     F)))))


;; (redisplay-frame-pane *application-frame*
;;                         (get-frame-pane *application-frame* 'render-output) :force-p t)
;; clim:make-rgb-color

(defun draw-circle (knot stream)
  (let* ((first-beadie (first knot))
         (x (+ (earth first-beadie) 200))
         (y (+ (wind first-beadie) 100)))
    (clim:with-drawing-options (stream :line-thickness 3 :line-cap-shape :round)
      (clim:draw-text* stream (format nil "test") 20 30)
      (clim:draw-circle* stream x y 25 :filled nil))))


(defun display-pusheens-home (frame pane)
  "How to display Pusheen's Home."
  (format pane "~a" (knot frame))
  (draw-circle (knot frame) pane)
  (when (untangledp (knot clim:*application-frame*))
    (format pane "Success!")))

;;; Commands and allowed moves

(define-pusheens-home-command (com-quit :menu t) ()
  (clim:frame-exit clim:*application-frame*))

(define-pusheens-home-command (com-play :menu t) ()
  (tangle-knot (knot clim:*application-frame*)))

(defmacro register-move (command section direction plane)
  "Register a new gameplay move."
  `(define-pusheens-home-command (,command :name t) ()
     (apply-move (knot clim:*application-frame*) ,section ,direction ,plane)))

;; Direction mappings swap from L to R, F to B, and D to U so that the transformations
;; can be independent of the plane.

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
  (let* ((gameplay-knot (tangle-knot (make-instance 'tiny-knot)))
         (frame (clim:make-application-frame 'pusheens-home :knot gameplay-knot)))
    (clim:run-frame-top-level frame)))
