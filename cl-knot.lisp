;;;; cl-knot.lisp
;;;;
;;;; Author: Lauren Capelluto

(in-package #:cl-knot)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Beadies ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass beadie ()
  ((solved-position :reader solved-pos
                    :documentation "A list specifying the untangled X Y Z positions of this beadie.")
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
  (setf (slot-value object 'solved-position)
        (list (x-pos object) (y-pos object) (z-pos object))))

(defmethod print-object ((object beadie) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~D:  ~D ~D ~D" (solved-pos object)
            (x-pos object)
            (y-pos object)
            (z-pos object))))

(defun solution-axis-difference (axis beadie0 beadie1)
  "Return the difference between the values of the solution position of the beadies along axis AXIS. AXIS is an integer, either 0=>X 1=>Y or 2=>Z."
  (abs (- (nth axis (solved-pos beadie0)) (nth axis (solved-pos beadie1)))))

(defun solution-difference (beadie0 beadie1)
  "Return the absolute difference of the beadie's solution position, per index."
  (loop :for axis :in '(0 1 2)
    :collect (solution-axis-difference axis beadie0 beadie1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Knots ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass knot ()
  ((beadies :accessor beadies
            :documentation "The beadies which are part of the knot.")
   (center :accessor center
           :documentation "Geometrically central point of the knot around which to rotate beadies."))
  (:documentation "The game knot from which we attempt to untangle Pusheen!"))

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

(defgeneric threadp (knot beadie0 beadie1)
  (:documentation "Is there a thread between the two beadies, i.e. are they neighbors?")

  (:method ((knot knot) beadie0 beadie1)
    (let ((differences (apply '+ (solution-difference beadie0 beadie1))))
      (<= differences 1))))

(defgeneric tangle-knot (knot)
  (:documentation "Randomly tangle the knot.")

  (:method ((knot knot))
    (let
        ((axes (list 'x 'y 'z))
         (pos (list 0 2))
         (directions (list 'pull 'push)))
      (dotimes (i 50)
        (apply-move knot
                    (make-plane  (nth (random 3) axes)
                                 (nth (random 2) pos))
                    (nth (random 2) directions))))
    knot))

(defgeneric untangledp (input)
  (:documentation "T if the input is in the solution state, NIL otherwise.")

  (:method ((beadie beadie))
    "Is the beadie in the solution state?"
    (equal (solved-pos beadie)
           (list (x-pos beadie) (y-pos beadie) (z-pos beadie))))

  (:method ((knot knot))
    "Is the knot untangled (i.e. in the solution state)?"
    (loop :for beadie :in (beadies knot)
          :when (not (untangledp beadie))
            :do (return nil)
          :finally (return t))))

(defmethod print-object ((object knot) stream)
  (print-unreadable-object (object stream :type t)
    (dolist (beadie (beadies object))
      (format stream "~a~%" beadie))))

(defstruct (point (:constructor make-point (x y z)))
  "A 3D point."
  (x 0 :type float)
  (y 0 :type float)
  (z 0 :type float))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Gameplay ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro make-axis-rotation (axis)
  "Define a generic function and method specialized on the knot class to rotate a given beadie pi/2 either clockwise or counterclockwise around AXIS. AXIS can be the symbol X, Y or Z.

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
       (:documentation "Rotate the given BEADIE around the center of the KNOT along the axis ,axis and in the direction DIRECTION.")
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
                 (floor (+ ax1-center
                           (* ax1-sign
                              (- ax2-beadie-pos ax2-center)))))
           (setf (,axis2-pos beadie)
                 (floor (+ ax2-center
                           (* ax2-sign
                              (- ax1-beadie-pos ax1-center))))))))))

(make-axis-rotation x)
(make-axis-rotation y)
(make-axis-rotation z)

(defgeneric apply-move (knot plane direction)
  (:documentation "Act a complete move upon the knot structure.")

  (:method ((knot knot) plane direction)
    (dolist (beadie (beadies knot))
      (cond ((and  (equal (plane-axis plane) 'x)
                   (= (plane-position plane) (x-pos beadie)))
             (rotate-x knot beadie direction))
            ((and (equal (plane-axis plane) 'y)
                  (= (plane-position plane) (y-pos beadie)))
             (rotate-y knot beadie direction))
            ((and (equal (plane-axis plane) 'z)
                  (= (plane-position plane) (z-pos beadie)))
             (rotate-z knot beadie direction)))))

  (:method ((knot tiny-knot) plane direction)
    ;; The normal dimension is 2, but the tiny knot is, well, tiny.
    (when (= (plane-position plane) 2)
      (call-next-method knot (make-plane (plane-axis plane) 1) direction))
    (call-next-method)))

(defstruct (plane (:constructor make-plane (axis position)))
  "An infinite 2D plane, perpendicular to the AXIS, X Y or Z, given by the POSITION along that axis."
  (axis 'x)
  (position 0 :type integer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Graphics ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *height* 400)
(defparameter *width* 700)
(defparameter *beadie-scale* 150)
(defparameter *beadie-size* 10)
(defparameter *x-offset* 500)
(defparameter *y-offset* 100)
(defparameter *rx-theta* (/ pi 5))
(defparameter *ry-theta* (/ (* 5 pi) 6))
(defparameter *rz-theta* 0)
(defparameter *teal* (clim:make-rgb-color (/ 10 225) (/ 212 255) (/ 212 255)))
(defparameter *light-gray* (clim:make-rgb-color (/ 200 225) (/ 232 255) (/ 232 255)))

(defun beadie-to-sheet-x (beadie)
  "Transform the beadie's X Y Z coordinates to the X coordinates of the sheet to be used by CLIM."
  (let* ((rotated-x (- (* (x-pos beadie) (cos *ry-theta*))
                       (* (z-pos beadie) (sin *ry-theta*))))
         (scaled-x (* *beadie-scale* rotated-x))
         (offset-x (+ scaled-x *x-offset*)))
    (floor offset-x)))

(defun beadie-to-sheet-y (beadie)
  "Transform the beadie's X Y Z coordinates to the Y coordinates of the sheet to be used by CLIM."
  (let* ((z (+ (* (x-pos beadie) (sin *ry-theta*))
               (* (z-pos beadie) (cos *ry-theta*))))
         (rotated-y (- (* (y-pos beadie) (cos *rx-theta*))
                       (* z (sin *rx-theta*))))
         (scaled-y (* *beadie-scale* rotated-y))
         (offset-y (+ scaled-y *y-offset*)))
    (floor offset-y)))

(defun draw-beadies (knot stream)
  "Draw each beadie, and fill it in with color if it's in its solved location."
  (dolist (beadie (beadies knot))
    (let ((x (beadie-to-sheet-x beadie))
          (y (beadie-to-sheet-y beadie))
          (fillp (untangledp beadie)))
      (clim:with-drawing-options (stream :line-thickness 3)
        (clim:draw-circle* stream x y *beadie-size* :filled fillp)
        (when fillp
          (clim:draw-circle* stream x y (- *beadie-size* 3) :filled t :ink *teal*))))))

(defun draw-threads (knot stream)
  "Draw threads between beadies which are adjacent."
  (dolist (beadie0 (beadies knot))
    (dolist (beadie1 (beadies knot))
      (when (threadp knot beadie0 beadie1)
        (let ((b0-sol (make-beadie (first (solved-pos beadie0))
                                   (second (solved-pos beadie0))
                                   (third (solved-pos beadie0))))
              (b1-sol (make-beadie (first (solved-pos beadie1))
                                   (second (solved-pos beadie1))
                                   (third (solved-pos beadie1)))))
          (clim:draw-line* stream
                           (beadie-to-sheet-x b0-sol)
                           (beadie-to-sheet-y b0-sol)
                           (beadie-to-sheet-x b1-sol)
                           (beadie-to-sheet-y b1-sol)
                           :ink *light-gray*))
        (clim:draw-line* stream
                         (beadie-to-sheet-x beadie0)
                         (beadie-to-sheet-y beadie0)
                         (beadie-to-sheet-x beadie1)
                         (beadie-to-sheet-y beadie1))))))

(defun display-pusheens-home (frame pane)
  "How to display Pusheen's home."
  (draw-threads (knot frame) pane)
  (draw-beadies (knot frame) pane)
  (let ((text "Use Rubik's cube notation to untangle the knot"))
    (when (untangledp (knot clim:*application-frame*))
      (setf text "Success!"))
    (clim:draw-text* pane text (/ *width* 3) *height*)))

(clim:define-application-frame pusheens-home ()
  ((knot :initarg :knot
         :accessor knot))
  (:panes
   (app :application
        :height *height*
        :width *width*
        :scroll-bars nil
        :display-function 'display-pusheens-home)
   (int :interactor
        :height (/ *height* 4)
        :width *width*)
   (F   :push-button
        :label "F"
        :activate-callback #'(lambda (x)
                               (declare (ignore x))
                               (format t "Hit the F button."))))
  (:layouts
   (default (clim:vertically ()
              app (clim:horizontally () int
                                     F)))))

;;; Commands and allowed moves

(define-pusheens-home-command (com-quit :menu t) ()
  (clim:frame-exit clim:*application-frame*))

(define-pusheens-home-command (com-play :menu t) ()
  (tangle-knot (knot clim:*application-frame*)))

(defmacro register-move (command axis pos direction)
  "Register a new gameplay move."
  `(define-pusheens-home-command (,command :name t) ()
     (apply-move (knot clim:*application-frame*) (make-plane ,axis ,pos) ,direction)))

;; Direction mappings swap from L to R, F to B, and D to U so that the transformations
;; can be independent of the plane.

(register-move com-L 'x 0 'pull)
(register-move com-Linv 'x 0 'push)
(register-move com-R 'x 2 'push)
(register-move com-Rinv 'x 2 'pull)
(register-move com-D 'y 0 'pull)
(register-move com-Dinv 'y 0 'push)
(register-move com-U 'y 2 'push)
(register-move com-Uinv 'y 2 'pull)
(register-move com-F 'z 0 'pull)
(register-move com-Finv 'z 0 'push)
(register-move com-B  'z 2 'pull)
(register-move com-Binv 'z 2 'push)


(defun show-welcome ()
  (format t "~&~
***********************************
*       W E L C O M E   T O       *~%~
*   P U S H E E N ' S   H O M E   *~%~
***********************************~%")
  nil)

(defun main (argv)
  "Help untangle Pusheen from her yarn!"
  (declare (ignore argv))
  (show-welcome)

  (let* ((gameplay-knot (tangle-knot (make-instance 'tiny-knot)))
         (frame (clim:make-application-frame 'pusheens-home :knot gameplay-knot)))
    (clim:run-frame-top-level frame)))
