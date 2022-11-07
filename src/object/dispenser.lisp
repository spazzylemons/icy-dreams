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
                                            :collision 'enemy
                                            :hitsprite *sprite-dispenser*))

(defun spawn-dispenser (x y)
  (let ((result (spawn *behavior-dispenser*)))
    (setf (game-object-x result) x)
    (setf (game-object-y result) y)
    ; choose direction based on location
    (if (< x 128) (setf (game-object-direction result) 'left))
    result))
