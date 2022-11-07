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

(defun update-hardened (obj)
  (cond ((equal (game-object-direction obj) 'left)
           (setf (game-object-xvel obj) -0.25)
           (setf (game-object-x obj) (- (game-object-x obj) 4.0))
           (when (or (left-collision obj) (not (game-object-grounded obj)))
             (setf (game-object-xvel obj) 0.25)
             (setf (game-object-direction obj) 'right))
           (setf (game-object-x obj) (+ (game-object-x obj) 4.0)))
        ((equal (game-object-direction obj) 'right)
           (setf (game-object-xvel obj) 0.25)
           (setf (game-object-x obj) (+ (game-object-x obj) 4.0))
           (when (or (right-collision obj) (not (game-object-grounded obj)))
             (setf (game-object-xvel obj) -0.25)
             (setf (game-object-direction obj) 'left))
           (setf (game-object-x obj) (- (game-object-x obj) 4.0)))))

(defparameter *hardened-sprites*
              (make-array '(2) :initial-contents (list *sprite-hardened-walk1*
                                                       *sprite-hardened-walk2*)))

(defun draw-hardened (obj)
  (draw-sprite (game-object-x obj)
               (game-object-y obj)
               (if (game-object-other-timer obj)
                 *sprite-hardened-hide*
                 (aref *hardened-sprites* (floor (* (game-object-anim-timer obj) 2))))
               (eql (game-object-direction obj) 'left)))

(setf *behavior-hardened* (make-object-bhv :update #'update-hardened
                                           :draw #'draw-hardened
                                           :collision 'enemy
                                           :hitsprite *sprite-hardened-walk1*))

(defun spawn-hardened (x y)
  (let ((result (spawn *behavior-hardened*)))
    (setf (game-object-x result) x)
    (setf (game-object-y result) y)
    result))
