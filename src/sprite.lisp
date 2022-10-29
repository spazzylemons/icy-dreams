(in-package :icy-dreams)

;; The spritesheet.
(defvar *spritesheet*)

;; Load the spritesheet. Must be done after opening the window.
(defun load-spritesheet ()
  (setf *spritesheet* (raylib:load-texture "assets/spritesheet.png")))

;; Draw a sprite at a given location. The sprite can be flipped horizontally.
(defun draw-sprite (x y id flipped)
  (let ((src (raylib:make-rectangle :x 0.0
                                    :y (* id 16.0)
                                    :width (if flipped -16.0 16.0)
                                    :height 16.0)))
    (raylib:draw-texture-rec *spritesheet*
                             src
                             (3d-vectors:vec (floor (- x 8.0)) (floor (+ y 8.0)))
                             raylib:+white+)))
