(in-package :icy-dreams)

(defun update-roller (obj)
  (cond ((equal (game-object-direction obj) 'left)
           (decf (game-object-x obj))
           (when (left-collision obj)
             (setf (game-object-yvel obj) -2.0)
             (incf (game-object-x obj))
             (setf (game-object-direction obj) 'right)))
        ((equal (game-object-direction obj) 'right)
           (incf (game-object-x obj))
           (when (right-collision obj)
             (setf (game-object-yvel obj) -2.0)
             (decf (game-object-x obj))
             (setf (game-object-direction obj) 'left)))))

(defun draw-roller (obj)
  (draw-sprite (game-object-x obj) (game-object-y obj) *sprite-roller1* nil))

(defparameter *behavior-roller* (make-object-bhv :update #'update-roller
                                                 :draw #'draw-roller
                                                 :collision 'enemy))

(defun spawn-roller (x y)
  (let ((result (spawn *behavior-roller*)))
    (setf (game-object-x result) x)
    (setf (game-object-y result) y)
    result))
