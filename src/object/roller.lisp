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

(defun update-roller (obj)
  (cond ((equal (game-object-direction obj) 'left)
           (setf (game-object-xvel obj) -0.75)
           (decf (game-object-x obj))
           (when (left-collision obj)
             (setf (game-object-yvel obj) -0.75)
             (setf (game-object-xvel obj) 0.75)
             (setf (game-object-direction obj) 'right))
           (incf (game-object-x obj)))
        ((equal (game-object-direction obj) 'right)
           (setf (game-object-xvel obj) 0.75)
           (incf (game-object-x obj))
           (when (right-collision obj)
             (setf (game-object-yvel obj) -0.75)
             (setf (game-object-xvel obj) -0.75)
             (setf (game-object-direction obj) 'left))
           (decf (game-object-x obj)))))

(defparameter *roller-sprites*
              (make-array '(2) :initial-contents (list *sprite-roller1*
                                                       *sprite-roller2*)))

(defun draw-roller (obj)
  (draw-sprite (game-object-x obj)
               (game-object-y obj)
               (aref *roller-sprites* (floor (* (game-object-anim-timer obj) 2)))
               nil))

(defparameter *behavior-roller* (make-object-bhv :update #'update-roller
                                                 :draw #'draw-roller
                                                 :collision 'enemy
                                                 :hitsprite *sprite-roller1*))

(defun spawn-roller (x y)
  (let ((result (spawn *behavior-roller*)))
    (setf (game-object-x result) x)
    (setf (game-object-y result) y)
    ; choose direction based on location
    (if (< x 128) (setf (game-object-direction result) 'left))
    result))
