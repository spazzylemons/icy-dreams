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
