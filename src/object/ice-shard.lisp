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

(defun update-ice-shard (obj)
  ; apply velocity to position
  (setf (game-object-x obj) (+ (game-object-x obj) (game-object-xvel obj)))
  (setf (game-object-y obj) (+ (game-object-y obj) (game-object-yvel obj)))
  (setf (game-object-yvel obj) (+ (game-object-yvel obj) *gravity*)))

(defun draw-ice-shard (obj)
  (draw-sprite (game-object-x obj) (game-object-y obj) *sprite-ice-shard* nil))

(defparameter *behavior-ice-shard* (make-object-bhv :update #'update-ice-shard
                                                    :draw #'draw-ice-shard
                                                    :collision 'none))

(defun spawn-ice-shard (ice)
  (let ((result (spawn *behavior-ice-shard*)))
    (setf (game-object-x result) (game-object-x ice))
    (setf (game-object-y result) (game-object-y ice))
    (setf (game-object-xvel result) (- (random 4.0) 2.0))
    (setf (game-object-yvel result) (- (random 2.0) 3.0))
    (setf (game-object-has-physics result) nil)
    (setf (game-object-despawn-timer result) 30)
    result))
