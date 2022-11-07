;; Icy Dreams - arcade-style platformer
;; Copyright (C) 2022 spazzylemons

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

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
  *sprite-player-death*
  *sprite-ice-cloud*
  *sprite-ice-block*
  *sprite-ice-shard*
  *sprite-roller1*
  *sprite-roller2*
  *sprite-spring-tall*
  *sprite-spring-short*
  *sprite-hardened-walk1*
  *sprite-hardened-walk2*
  *sprite-hardened-hide*
  *sprite-bat-hang*
  *sprite-bat-flap1*
  *sprite-bat-flap2*
  *sprite-dispenser*)

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
