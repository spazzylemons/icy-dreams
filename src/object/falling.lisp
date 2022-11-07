(in-package :icy-dreams)

(defun update-falling (obj)
  ; apply y velocity
  (setf (game-object-y obj) (+ (game-object-y obj) (game-object-yvel obj)))
  ; gravity
  (setf (game-object-yvel obj) (+ (game-object-yvel obj) *gravity*))
  (cond ((> (game-object-yvel obj) *terminal-velocity*)
         (setf (game-object-yvel obj) *terminal-velocity*))
        ((< (game-object-yvel obj) (- *terminal-velocity*))
         (setf (game-object-yvel obj) (- *terminal-velocity*))))
  ; despawn on offscreen
  (when (> (game-object-y obj) 208) (despawn obj)))

(defun draw-falling (obj)
  (draw-sprite-helper (game-object-x obj)
                      (game-object-y obj)
                      (game-object-dead obj)
                      nil))

(setf *behavior-falling* (make-object-bhv :update #'update-falling
                                          :draw #'draw-falling
                                          :collision 'none))

(defun spawn-falling (x y sprite)
  (let ((result (spawn *behavior-falling*)))
    (setf (game-object-x result) x)
    (setf (game-object-y result) y)
    (setf (game-object-yvel result) -2.0)
    (setf (game-object-has-physics result) nil)
    (setf (game-object-dead result) sprite)
    result))
