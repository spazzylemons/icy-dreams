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

(defparameter *player-accel* 0.25)
(defparameter *player-maxvel* 1.0)

(defparameter *player-object* nil)

(defun update-player (obj)
  ; if dead, do death logic
  (if (game-object-dead obj)
    (progn
      (when (= (game-object-iframes obj) 1)
        (when (= *num-lives* 0)
          (signal 'game-lost))
        (decf *num-lives*)
        (spawn-player 120)
        (despawn obj)))
    (progn
      (when (raylib:is-key-down raylib:+key-left+)
        (setf (game-object-direction obj) 'left)
        (setf (game-object-xvel obj) (- (game-object-xvel obj) *player-accel*)))
      (when (raylib:is-key-down raylib:+key-right+)
        (setf (game-object-direction obj) 'right)
        (setf (game-object-xvel obj) (+ (game-object-xvel obj) *player-accel*)))
      (when (and (game-object-grounded obj) (raylib:is-key-pressed raylib:+key-x+))
        (raylib:play-sound *sfx-jump*)
        (setf (game-object-yvel obj) (- *terminal-velocity*)))
      (when (and (raylib:is-key-pressed raylib:+key-z+) (not (game-object-other-timer obj)))
        (raylib:play-sound *sfx-attack*)
        ; set anim timer for cooldown
        (setf (game-object-other-timer obj) 5)
        ; are we holding ice?
        (if (game-object-throw obj)
          ; holding ice, throw
          (progn
            ; spawn ice block!
            (throw-ice obj)
            (setf (game-object-throw obj) nil))
          ; not holding ice, try to find some
          (progn
            (block find-ice
              (dolist (ice *game-objects*)
                (when (and (object-collision obj ice)
                           (equal (object-bhv-collision (game-object-bhv ice)) 'ice-block)
                           (not (game-object-throw ice)))
                  ; pick up the ice
                  ; TODO potential race condition in future if enemies can escape ice
                  ; when that happens, don't pick up the ice if enemy about to escape
                  ; ...unless the player is always last to be updated?
                  (setf (game-object-throw obj) t)
                  (despawn ice)
                  (return-from find-ice)))
              ; found no ice, spawn ice cloud instead
              (spawn-ice-cloud obj)))))
      (cond ((> (game-object-xvel obj) *player-maxvel*) (setf (game-object-xvel obj) *player-maxvel*))
            ((< (game-object-xvel obj) (- *player-maxvel*)) (setf (game-object-xvel obj) (- *player-maxvel*)))))))

(defparameter *player-walk-sprites*
              (make-array '(4) :initial-contents (list *sprite-player-walk1*
                                                       *sprite-player-idle*
                                                       *sprite-player-walk2*
                                                       *sprite-player-idle*)))

(defun draw-player (obj)
  (unless (game-object-dead obj)
    (when (< (mod (game-object-iframes obj) 8) 4)
      (when (game-object-throw obj)
        (draw-sprite (game-object-x obj) (- (game-object-y obj) 16.0) *sprite-ice-block* nil))
      (let ((sprite (cond ((and (game-object-other-timer obj) (not (game-object-throw obj))) *sprite-player-attack*)
                          ((not (game-object-grounded obj)) *sprite-player-walk2*)
                          ((= (game-object-xvel obj) 0.0) *sprite-player-idle*)
                          (t (aref *player-walk-sprites* (floor (* (game-object-anim-timer obj) 4)))))))
        (when (game-object-throw obj)
          (setq sprite (+ sprite *sprite-player-throw-idle*)))
        (draw-sprite (game-object-x obj) (game-object-y obj) sprite (eql (game-object-direction obj) 'left))))))

(defparameter *behavior-player* (make-object-bhv :update #'update-player
                                                 :draw #'draw-player
                                                 :collision 'player))

(defun spawn-player (iframes)
  (let ((result (spawn *behavior-player*)))
    (setf *player-object* result)
    (setf (game-object-x result) 32.0)
    (setf (game-object-y result) 172.0)
    (setf (game-object-iframes result) iframes)))

(defun kill-player (obj)
  ; use iframes for timing when to respawn
  (setf (game-object-iframes obj) 60)
  (setf (game-object-dead obj) t)
  (setf (game-object-has-physics obj) nil)
  ; if holding ice block, drop it
  (when (game-object-throw obj)
    (drop-ice (game-object-x obj) (- (game-object-y obj) 8)))
  ; spawn falling object for death anim
  (spawn-falling (game-object-x obj) (game-object-y obj) *sprite-player-death*))
