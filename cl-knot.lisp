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
               :documentation "The beadie location on the z-axis."))
  (:documentation "A point in space that has a \"home\" position."))

(defun make-beadie (x y z)
  (assert (every #'numberp (list x y z)))
  (make-instance 'beadie :x-position x :y-position y :z-position z))

(defmethod initialize-instance :after ((object beadie) &key)
  (setf (slot-value object 'solved-position)
        (list (x-pos object) (y-pos object) (z-pos object))))

(defmethod print-object ((object beadie) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~D:  ~D ~D ~D"
            (solved-pos object)
            (x-pos object)
            (y-pos object)
            (z-pos object))))

(defun solution-difference (beadie0 beadie1)
  "Return the vector absolute difference of the beadies' solved position."
  (check-type beadie0 beadie)
  (check-type beadie1 beadie)
  (loop :for a :in (solved-pos beadie0)
        :for b :in (solved-pos beadie1)
        ;; Collect the absolute difference for X Y and Z axes
        :collect (abs (- a b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Knots ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass knot ()
  ((beadies :initarg beadies
            :accessor beadies
            :documentation "The beadies which are part of the knot.")
   (center :initarg :center
           :accessor center
           :documentation "Geometrically central point of the knot around which to rotate beadies."))
  (:documentation "The game knot from which we attempt to untangle Pusheen!"))

(defclass tiny-knot (knot)
  ()
  (:default-initargs
   :beadies (make-tiny-knot)
   :center (make-point 0.5 0.5 0.5)))

(defun make-tiny-knot ()
  "Make a list of just 8 beadies, matching a 2x2x2 cube."
  (let ((knot nil))
    (dotimes (x 2 knot)
      (dotimes (y 2)
        (dotimes (z 2)
          (push (make-beadie x y z) knot))))))

(defclass simple-knot (knot)
  ()
  (:default-initargs
   :beadies (make-simple-knot)
    :center (make-point 1.0 1.0 1.0)))

(defun base3->list (num)
  "Convert an integer interpreted in base 3 to a list of 3 integers, starting with the most significant."
  (reverse (loop :repeat 3
                 :collect (mod num 3)
                 :do (setf num (floor num 3)))))

(defun list->base3 (list)
  "Inverse of BASE3->LIST."
  (reduce (lambda (num d)
            (+ d (* 3 num)))
          list))

(defun simple-beadie-qualities-p (qualities)
  "Does this collection of qualities belong in the simple knot? The simple knot excludes middle pieces, so a beadie is a simple beadie only if its position has at most one coordinate valued 1. E.g. (0 1 2) is a simple beadie, but but (0 1 1) is not."
  (assert (every #'numberp qualities))
  (<= (count 1 qualities) 1))

(defun make-simple-knot ()
  "Make a list of beadies roughly patterned after a void cube."
  (loop :for base3 :from #3r000 :to #3r222
        :for qualities := (base3->list base3)
        :when (simple-beadie-qualities-p qualities)
          :collect (apply #'make-beadie qualities)))

(defgeneric thread-between-beadies-p (knot beadie0 beadie1)
  (:documentation "Is there a thread between the two beadies, i.e. are they neighbors?")

  (:method ((knot knot) beadie0 beadie1)
    (let ((differences (reduce #'+ (solution-difference beadie0 beadie1))))
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
    ;; Is the beadie in the solution state?
    (equal (solved-pos beadie)
           (list (x-pos beadie) (y-pos beadie) (z-pos beadie))))

  (:method ((knot knot))
    ;; Is the knot untangled (i.e. in the solution state)?
    (every #'untangledp (beadies knot))))

(defmethod print-object ((object knot) stream)
  (print-unreadable-object (object stream :type t)
    (dolist (beadie (beadies object))
      (format stream "~a~%" beadie))))

(defstruct (point (:constructor make-point (x y z)))
  "A 3D point."
  (x 0 :type real :read-only t)
  (y 0 :type real :read-only t)
  (z 0 :type real :read-only t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Gameplay ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; EVAL-WHEN so it can be used in macroexpansions below.
(eval-when (:compile-toplevel :load-toplevel :execute)
   (defun orthogonal (axis)
     "The first and second axis such that axis1 curl axis2 is AXIS"
     (ecase axis
       (x (values 'y 'z))
       (y (values 'z 'x))
       (z (values 'x 'y)))))

(defmacro define-axis-rotation (func-name axis)
  "Define a generic function and method specialized on the knot class to rotate a given beadie pi/2 either clockwise or counterclockwise around AXIS. AXIS can be the symbol X, Y or Z.

The defined function is called ROTATE-AXIS where AXIS is replaced by its value, and it takes a KNOT instance, BEADIE instance, and DIRECTION (either 'pull or 'push) as input."
  (check-type axis (member x y z))
  (multiple-value-bind (axis1 axis2) (orthogonal axis)
    (let ((axis1-pos (intern (format nil "~a-POS" axis1)))
          (axis2-pos (intern (format nil "~a-POS" axis2))))
      `(defgeneric ,func-name (knot beadie direction)
         (:documentation ,(format nil "Rotate the given BEADIE around the center of the KNOT along the axis ~A and in the direction DIRECTION." axis))
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
                                (- ax1-beadie-pos ax1-center)))))))))))

(define-axis-rotation rotate-x x)
(define-axis-rotation rotate-y y)
(define-axis-rotation rotate-z z)

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
  (axis 'x    :type (member x y z) :read-only t)
  (position 0 :type integer        :read-only t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Graphics ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *height* 400)
(defparameter *width* 700)
(defparameter *beadie-scale* 150)
(defparameter *beadie-size* 10)
(defparameter *x-offset* 450)
(defparameter *y-offset* 100)
(defparameter *rx-theta* (/ pi 5))
(defparameter *ry-theta* (/ (* 5 pi) 6))
(defparameter *teal* (clim:make-rgb-color (/ 10 225) (/ 212 255) (/ 212 255)))
(defparameter *light-gray* (clim:make-rgb-color (/ 200 225) (/ 232 255) (/ 232 255)))
(defparameter *dark-gray* (clim:make-rgb-color (/ 80 225) (/ 102 255) (/ 102 255)))

(defun beadie-to-sheet-x (beadie)
  "Transform the beadie's X Y Z coordinates to the X coordinates of the sheet to be used by CLIM."
  (check-type beadie beadie)
  (let* ((rotated-x (- (* (x-pos beadie) (cos *ry-theta*))
                       (* (z-pos beadie) (sin *ry-theta*))))
         (scaled-x (* *beadie-scale* rotated-x))
         (offset-x (+ scaled-x *x-offset*)))
    (floor offset-x)))

(defun beadie-to-sheet-y (beadie)
  "Transform the beadie's X Y Z coordinates to the Y coordinates of the sheet to be used by CLIM."
  (check-type beadie beadie)
  (let* ((z (+ (* (x-pos beadie) (sin *ry-theta*))
               (* (z-pos beadie) (cos *ry-theta*))))
         (rotated-y (- (* (y-pos beadie) (cos *rx-theta*))
                       (* z (sin *rx-theta*))))
         (scaled-y (* *beadie-scale* rotated-y))
         (offset-y (+ scaled-y *y-offset*)))
    (floor offset-y)))

(defun draw-beadies (knot stream)
  "Draw each beadie, and fill it in with color if it's in its solved location."
  (check-type knot knot)
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
  (check-type knot knot)
  (dolist (beadie0 (beadies knot))
    (dolist (beadie1 (beadies knot))
      (when (thread-between-beadies-p knot beadie0 beadie1)
	(clim:draw-line* stream
			 (beadie-to-sheet-x beadie0)
			 (beadie-to-sheet-y beadie0)
			 (beadie-to-sheet-x beadie1)
			 (beadie-to-sheet-y beadie1)
			 :line-thickness 2)))))

(defun draw-pusheen (stream)
  "Draw a little Pusheen."
  (clim:with-drawing-options (stream :line-thickness 3)
    ;; Ears
    (clim:draw-line* stream 45 30 26 53)
    (clim:draw-line* stream 45 30 60 45)
    (clim:draw-line* stream 60 45 90 45)
    (clim:draw-line* stream 90 45 105 30)
    (clim:draw-line* stream 105 30 126 56)
    ;; Stripes
    (clim:draw-circle* stream 63 45 6 :start-angle pi :end-angle 0
                                      :ink *dark-gray*)
    (clim:draw-circle* stream 75 45 6 :start-angle pi :end-angle 0
                                      :ink *dark-gray*)
    (clim:draw-circle* stream 87 45 6 :start-angle pi :end-angle 0
                                      :ink *dark-gray*)
    ;; Eyes: left and right
    (clim:draw-circle* stream 48 67 6 :filled t)
    (clim:draw-circle* stream 102 67 6 :filled t)
    ;; Nose
    (clim:draw-circle* stream 82 67 7 :start-angle (+ pi 0.5)
                                      :end-angle (* -1 (/ pi 3))
                                      :filled nil)
    (clim:draw-circle* stream 69 67 7 :start-angle (* 4 (/ pi 3))
                                      :end-angle (/ pi 8)
                                      :filled nil)
    ;; Whiskers: 2 left and 2 right
    (clim:draw-circle* stream 11 138 75 :start-angle (* 5 (/ pi 12))
                                      :end-angle (/ pi 2)
                                      :filled nil)
    (clim:draw-circle* stream 30 150 75 :start-angle (/ pi 2)
                                        :end-angle (* 7 (/ pi 12))
                                        :filled nil)
    (clim:draw-circle* stream 144 138 75 :start-angle (/ pi 2)
                                       :end-angle (* 7 (/ pi 12))
                                       :filled nil)
    (clim:draw-circle* stream 125 150 75 :start-angle (* 5 (/ pi 12))
                                        :end-angle (/ pi 2)
                                        :filled nil)))

(defun display-pusheens-home (frame pane)
  "How to display Pusheen's home."
  (draw-threads (knot frame) pane)
  (draw-beadies (knot frame) pane)
  (draw-pusheen pane)
  (clim:draw-text* pane "F" (/ *width* 2) (/ *height* 1.8))
  (clim:draw-text* pane "L" (/ *width* 2.1) (/ *height* 2.4))
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
        :width *width*))
  (:layouts
   (default (clim:vertically ()
              app (clim:horizontally () int)))))

;;; Commands and allowed moves

(define-pusheens-home-command (com-quit :menu t) ()
  (clim:frame-exit clim:*application-frame*))

(define-pusheens-home-command (com-play :menu t) ()
  (tangle-knot (knot clim:*application-frame*)))

(defmacro register-move (command axis pos direction)
  "Register a new gameplay move."
  `(define-pusheens-home-command (,command :name t) ()
     (apply-move (knot clim:*application-frame*) (make-plane ',axis ,pos) ',direction)))

;; Define all the gameplay moves, using standard Rubik's cube notation. Direction mappings swap from L to R, F to B, and D to U so that the transformations can be independent of the plane.

(register-move com-R    x 0 pull)
(register-move com-Rinv x 0 push)
(register-move com-L    x 2 push)
(register-move com-Linv x 2 pull)
(register-move com-U    y 0 pull)
(register-move com-Uinv y 0 push)
(register-move com-D    y 2 push)
(register-move com-Dinv y 2 pull)
(register-move com-B    z 0 pull)
(register-move com-Binv z 0 push)
(register-move com-F    z 2 push)
(register-move com-Finv z 2 pull)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Entry ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
