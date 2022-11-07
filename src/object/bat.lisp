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

(defun is-bat-hanging (obj)
  (decf (game-object-y obj))
  (let ((result (up-collision obj)))
    (incf (game-object-y obj))
    result))

(defun find-player ()
  (block finder
    (dolist (obj *game-objects*)
      (when (eql (game-object-bhv obj) *behavior-player*)
        (return-from finder obj)))
    nil))

(defun update-bat (obj)
  ; find player
  (if (is-bat-hanging obj)
    (let ((player (find-player)))
      (setf (game-object-yvel obj) 0.0)
      (when player
        (let ((dx (- (game-object-x obj) (game-object-x player)))
              (dy (- (game-object-y obj) (game-object-y player))))
          (if (< (+ (* dx dx) (* dy dy)) 4096)
            (progn
              (incf (game-object-y obj))
              (setf (game-object-yvel obj) 2.5)
              (cond ((< (game-object-x player) (game-object-x obj))
                       (setf (game-object-xvel obj) -0.5))
                    ((> (game-object-x player) (game-object-x obj))
                       (setf (game-object-xvel obj) 0.5))
                    (t
                       (setf (game-object-xvel obj) 0.0))))
            (setf (game-object-xvel obj) 0.0)))))
    (setf (game-object-yvel obj) (- (game-object-yvel obj) 0.0625))))

(defparameter *bat-sprites*
              (make-array '(2) :initial-contents (list *sprite-bat-flap1*
                                                       *sprite-bat-flap2*)))

(defun draw-bat (obj)
  (draw-sprite (game-object-x obj)
               (game-object-y obj)
               (if (is-bat-hanging obj)
                 *sprite-bat-hang*
                 (aref *bat-sprites* (floor (* (game-object-anim-timer obj) 2))))
               nil))

(setf *behavior-bat* (make-object-bhv :update #'update-bat
                                      :draw #'draw-bat
                                      :collision 'enemy
                                      :hitsprite *sprite-bat-flap1*))

(defun spawn-bat (x y)
  (let ((result (spawn *behavior-bat*)))
    (setf (game-object-x result) x)
    (setf (game-object-y result) y)
    result))
