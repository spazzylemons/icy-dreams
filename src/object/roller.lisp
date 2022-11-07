(in-package :icy-dreams)

(defun update-roller (obj)
  (cond ((equal (game-object-direction obj) 'left)
           (setf (game-object-xvel obj) -0.75)
           (decf (game-object-x obj))
           (when (left-collision obj)
             (setf (game-object-yvel obj) -0.75)
             (setf (game-object-xvel obj) 0.75)
             (setf (game-object-direction obj) 'right))
           (incf (game-object-x obj)))
        ((equal (game-object-direction obj) 'right)
           (setf (game-object-xvel obj) 0.75)
           (incf (game-object-x obj))
           (when (right-collision obj)
             (setf (game-object-yvel obj) -0.75)
             (setf (game-object-xvel obj) -0.75)
             (setf (game-object-direction obj) 'left))
           (decf (game-object-x obj)))))

(defparameter *roller-sprites*
              (make-array '(2) :initial-contents (list *sprite-roller1*
                                                       *sprite-roller2*)))

(defun draw-roller (obj)
  (draw-sprite (game-object-x obj)
               (game-object-y obj)
               (aref *roller-sprites* (floor (* (game-object-anim-timer obj) 2)))
               nil))

(defparameter *behavior-roller* (make-object-bhv :update #'update-roller
                                                 :draw #'draw-roller
                                                 :collision 'enemy
                                                 :hitsprite *sprite-roller1*))

(defun spawn-roller (x y)
  (let ((result (spawn *behavior-roller*)))
    (setf (game-object-x result) x)
    (setf (game-object-y result) y)
    ; choose direction based on location
    (if (< x 128) (setf (game-object-direction result) 'left))
    result))
