(in-package :icy-dreams)

(defun update-springy (obj)
  (cond ((equal (game-object-direction obj) 'left)
           (setf (game-object-xvel obj) -0.75)
           (decf (game-object-x obj))
           (when (left-collision obj)
             (setf (game-object-xvel obj) 0.75)
             (setf (game-object-direction obj) 'right))
           (incf (game-object-x obj)))
        ((equal (game-object-direction obj) 'right)
           (setf (game-object-xvel obj) 0.75)
           (incf (game-object-x obj))
           (when (right-collision obj)
             (setf (game-object-xvel obj) -0.75)
             (setf (game-object-direction obj) 'left))
           (decf (game-object-x obj))))
  (when (game-object-grounded obj)
    (setf (game-object-yvel obj) -1.75)
    (if (< (game-object-x *player-object*) (game-object-x obj))
      (setf (game-object-direction obj) 'left)
      (setf (game-object-direction obj) 'right))))

(defun draw-springy (obj)
  (draw-sprite (game-object-x obj)
               (game-object-y obj)
               (if (>= (game-object-yvel obj) 0) *sprite-spring-short* *sprite-spring-tall*)
               nil))

(defparameter *behavior-springy* (make-object-bhv :update #'update-springy
                                                 :draw #'draw-springy
                                                 :collision 'enemy
                                                 :hitsprite *sprite-spring-tall*))

(defun spawn-springy (x y)
  (let ((result (spawn *behavior-springy*)))
    (setf (game-object-x result) x)
    (setf (game-object-y result) y)
    ; choose direction based on location
    (if (< x 128) (setf (game-object-direction result) 'left))
    result))
