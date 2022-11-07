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

(defun update-ice-cloud (obj)
  (cond ((equal (game-object-direction obj) 'left)
         (setf (game-object-x obj) (- (game-object-x obj) 3.0))
         (when (left-collision obj)
           (despawn obj)))
        ((equal (game-object-direction obj) 'right)
         (setf (game-object-x obj) (+ (game-object-x obj) 3.0))
         (when (right-collision obj)
           (despawn obj)))))

(defun draw-ice-cloud (obj)
  (draw-sprite (game-object-x obj) (game-object-y obj) *sprite-ice-cloud* nil))

(defparameter *behavior-ice-cloud* (make-object-bhv :update #'update-ice-cloud
                                                    :draw #'draw-ice-cloud
                                                    :collision 'attack))

(defun spawn-ice-cloud (player)
  (let ((result (spawn *behavior-ice-cloud*)))
    (setf (game-object-has-physics result) nil)
    (setf (game-object-x result) (game-object-x player))
    (setf (game-object-y result) (game-object-y player))
    (setf (game-object-direction result) (game-object-direction player))
    (setf (game-object-despawn-timer result) 30)))
