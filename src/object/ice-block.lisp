(in-package :icy-dreams)

(defun shatter-ice-block (obj)
  (spawn-ice-shard obj)
  (spawn-ice-shard obj)
  (spawn-ice-shard obj)
  (add-score (game-object-throw obj))
  (despawn obj))

(defun update-ice-block (obj)
  ; only special logic if thrown
  (when (game-object-throw obj)
    ; half the effects of gravity
    ; super mario bros. 2, for example, uses much lower gravity for thrown
    ; objects, making it easier to aim
    (setf (game-object-y obj) (- (game-object-y obj) (/ (game-object-yvel obj) 2.0)))
    ; if hit a wall, then shatter
    (when (game-object-hit-wall obj)
      (shatter-ice-block obj))))

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
 
(defun throw-ice (player)
  (let ((result (spawn *behavior-ice-block*)))
    (setf (game-object-x result) (game-object-x player))
    (setf (game-object-y result) (- (game-object-y player) 8.0))
    (setf (game-object-xvel result) (if (equal (game-object-direction player) 'left) -2.0 2.0))
    (setf (game-object-yvel result) -0.5)
    (setf (game-object-throw result) 200)
    result))
