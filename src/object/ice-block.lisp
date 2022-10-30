(in-package :icy-dreams)

(defun update-ice-block (obj)
  nil)

(defun draw-ice-block (obj)
  (draw-sprite (game-object-x obj) (game-object-y obj) *sprite-ice-block* nil))

(defparameter *behavior-ice-block* (make-object-bhv :update #'update-ice-block
                                                    :draw #'draw-ice-block
                                                    :collision 'ice-block))

(defun turn-to-ice (obj)
  (let ((result (spawn *behavior-ice-block*)))
    (setf (game-object-x result) (game-object-x obj))
    (setf (game-object-y result) (game-object-y obj))
    (setf (game-object-xvel result) (game-object-xvel obj))
    (setf (game-object-yvel result) (game-object-yvel obj))
    (setf (game-object-grounded result) (game-object-grounded obj))
    (despawn obj)
    result))
