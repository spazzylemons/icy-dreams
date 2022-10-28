(in-package :icy-dreams)

(defun update-player (obj)
  (if (raylib:is-key-down raylib:+key-left+)
    (decf (game-object-x obj)))
  (if (raylib:is-key-down raylib:+key-right+)
    (incf (game-object-x obj)))
  (if (raylib:is-key-down raylib:+key-up+)
    (decf (game-object-y obj)))
  (if (raylib:is-key-down raylib:+key-down+)
    (incf (game-object-y obj))))

(defun draw-player (obj)
  (draw-sprite (game-object-x obj) (game-object-y obj) 0 nil))
