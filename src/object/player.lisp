(in-package :icy-dreams)

(defparameter *player-accel* 0.25)
(defparameter *player-maxvel* 1.0)

(defun update-player (obj)
  ; (unless (collision (game-object-x obj) (game-object-y obj))
  (when (raylib:is-key-down raylib:+key-left+)
    (setf (game-object-xvel obj) (- (game-object-xvel obj) *player-accel*)))
  (when (raylib:is-key-down raylib:+key-right+)
    (setf (game-object-xvel obj) (+ (game-object-xvel obj) *player-accel*)))
  (when (and (game-object-grounded obj) (raylib:is-key-pressed raylib:+key-space+))
    (setf (game-object-yvel obj) (- *terminal-velocity*)))
  (cond ((> (game-object-xvel obj) *player-maxvel*) (setf (game-object-xvel obj) *player-maxvel*))
        ((< (game-object-xvel obj) (- *player-maxvel*)) (setf (game-object-xvel obj) (- *player-maxvel*)))))

(defun draw-player (obj)
  (draw-sprite (game-object-x obj) (game-object-y obj) 0 nil))

(defparameter *behavior-player* (make-object-bhv :update #'update-player :draw #'draw-player))

(defun spawn-player ()
  (spawn *behavior-player*))
