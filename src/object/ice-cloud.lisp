(in-package :icy-dreams)

(defun update-ice-cloud (obj)
  (cond ((equal (game-object-direction obj) 'left)
         (setf (game-object-x obj) (- (game-object-x obj) 3.0))
         (when (left-collision obj)
           (despawn obj)))
        ((equal (game-object-direction obj) 'right)
         (setf (game-object-x obj) (+ (game-object-x obj) 3.0))
         (when (right-collision obj)
           (despawn obj)))))

(defun draw-ice-cloud (obj)
  (draw-sprite (game-object-x obj) (game-object-y obj) *sprite-ice-cloud* nil))

(defparameter *behavior-ice-cloud* (make-object-bhv :update #'update-ice-cloud :draw #'draw-ice-cloud))

(defun spawn-ice-cloud (player)
  (let ((result (spawn *behavior-ice-cloud*)))
    (setf (game-object-has-physics result) nil)
    (setf (game-object-x result) (game-object-x player))
    (setf (game-object-y result) (game-object-y player))
    (setf (game-object-direction result) (game-object-direction player))
    (setf (game-object-despawn-timer result) 20)))
