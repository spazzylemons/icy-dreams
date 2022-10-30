(in-package :icy-dreams)

;; The spritesheet.
(defvar *spritesheet*)

;; The sprite ID counter.
(defparameter *sprite-counter* 0)

;; Get a new sprite ID.
(defun next-sprite ()
  (let ((result *sprite-counter*))
    (incf *sprite-counter*)
    result))

(defmacro spritesheet-enum (&rest rest) rest
  (let ((s 0)
        (result nil))
    (dolist (i rest)
      (push `(defparameter ,i ,s) result)
      (incf s))
    (push 'progn result)
    result))

;; Sprite IDs.
(spritesheet-enum
  *sprite-player-idle*
  *sprite-player-walk1*
  *sprite-player-walk2*
  *sprite-player-attack*
  *sprite-player-throw-idle*
  *sprite-player-throw-walk1*
  *sprite-player-throw-walk2*
  *sprite-player-throw-attack*
  *sprite-ice-cloud*
  *sprite-ice-block*
  *sprite-ice-shard*
  *sprite-roller1*
  *sprite-roller2*)

;; Load the spritesheet. Must be done after opening the window.
(defun load-spritesheet ()
  (setf *spritesheet* (raylib:load-texture "assets/spritesheet.png")))

;; Draw a sprite, not repeating it for wraparound.
(defun draw-sprite-helper (x y id flipped)
  (let ((src (raylib:make-rectangle :x 0.0
                                    :y (* id 16.0)
                                    :width (if flipped -16.0 16.0)
                                    :height 16.0)))
    (raylib:draw-texture-rec *spritesheet*
                             src
                             (3d-vectors:vec (floor (- x 8.0)) (floor (+ y 8.0)))
                             raylib:+white+)))

;; Draw a sprite at a given location. The sprite can be flipped horizontally.
(defun draw-sprite (x y id flipped)
  (draw-sprite-helper x (- y (* *stage-height* 8)) id flipped)
  (draw-sprite-helper x y id flipped)
  (draw-sprite-helper x (+ y (* *stage-height* 8)) id flipped))
