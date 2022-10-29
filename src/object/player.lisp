(in-package :icy-dreams)

(defparameter *player-accel* 0.25)
(defparameter *player-maxvel* 1.0)

(defun update-player (obj)
  ; (unless (collision (game-object-x obj) (game-object-y obj))
  (when (raylib:is-key-down raylib:+key-left+)
    (setf (game-object-direction obj) 'left)
    (setf (game-object-xvel obj) (- (game-object-xvel obj) *player-accel*)))
  (when (raylib:is-key-down raylib:+key-right+)
    (setf (game-object-direction obj) 'right)
    (setf (game-object-xvel obj) (+ (game-object-xvel obj) *player-accel*)))
  (when (and (game-object-grounded obj) (raylib:is-key-pressed raylib:+key-space+))
    (setf (game-object-yvel obj) (- *terminal-velocity*)))
  (cond ((> (game-object-xvel obj) *player-maxvel*) (setf (game-object-xvel obj) *player-maxvel*))
        ((< (game-object-xvel obj) (- *player-maxvel*)) (setf (game-object-xvel obj) (- *player-maxvel*)))))

(defparameter *player-walk-sprites*
              (make-array '(4) :initial-contents (list *sprite-player-walk1*
                                                       *sprite-player-idle*
                                                       *sprite-player-walk2*
                                                       *sprite-player-idle*)))

(defun draw-player (obj)
  (let ((sprite (cond ((not (game-object-grounded obj)) *sprite-player-walk2*)
                      ((= (game-object-xvel obj) 0.0) *sprite-player-idle*)
                      (t (aref *player-walk-sprites* (floor (* (game-object-anim-timer obj) 4)))))))
    (draw-sprite (game-object-x obj) (game-object-y obj) sprite (eql (game-object-direction obj) 'left))))

(defparameter *behavior-player* (make-object-bhv :update #'update-player :draw #'draw-player))

(defun spawn-player ()
  (spawn *behavior-player*))
