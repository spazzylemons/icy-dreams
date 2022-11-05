(in-package :icy-dreams)

(defun update-dispenser (obj)
  ; timer is nil? dispense roller
  ; but not if we've got over 15 objects already in scene, as to not overwhelm
  (when (and (not (game-object-other-timer obj)) (<= (length *game-objects*) 15))
    (let ((roller (spawn-roller (game-object-x obj) (game-object-y obj))))
      ; copy direction
      (setf (game-object-direction roller) (game-object-direction obj)))
    ; flip direction
    (setf (game-object-direction obj) (if (equal (game-object-direction obj) 'left) 'right 'left))
    ; reset timer
    (setf (game-object-other-timer obj) 240)))

(defun draw-dispenser (obj)
  (draw-sprite (game-object-x obj)
               (game-object-y obj)
               *sprite-dispenser*
               nil))

(setf *behavior-dispenser* (make-object-bhv :update #'update-dispenser
                                            :draw #'draw-dispenser
                                            :collision 'enemy))

(defun spawn-dispenser (x y)
  (let ((result (spawn *behavior-dispenser*)))
    (setf (game-object-x result) x)
    (setf (game-object-y result) y)
    ; choose direction based on location
    (if (< x 128) (setf (game-object-direction result) 'left))
    result))
