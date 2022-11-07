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

;; Forward declaration for hardened special case.
(defparameter *behavior-hardened* nil)

;; Forward declaration for bat special case.
(defparameter *behavior-bat* nil)

;; Forward declaration for dispenser special case.
(defparameter *behavior-dispenser* nil)

;; Forward declaration for falling special case.
(defparameter *behavior-falling* nil)

;; The list of game objects.
(defparameter *game-objects* nil)

;; The list of game objects pending for spawning.
(defparameter *pending-objects* nil)

;; The list of game objects pending for despawning.
(defparameter *pending-despawns* nil)

;; A timer for advancing to the next stage.
(defparameter *stage-advance-timer* nil)

;; Number of lives.
(defparameter *num-lives* 2)

;; Reset the object system.
(defun reset-objects ()
  (setf *game-objects* nil)
  (setf *pending-objects* nil)
  (setf *pending-despawns* nil))

;; Spawn a game object. Returns the object.
(defun spawn (bhv)
  (let ((result (make-game-object
                  :x 0.0
                  :y 0.0
                  :xvel 0.0
                  :yvel 0.0
                  :grounded nil
                  :has-physics t
                  :anim-timer 0
                  :direction 'right
                  :despawn-timer nil
                  :other-timer nil
                  :throw nil
                  :hit-wall nil
                  :iframes 0
                  :dead nil
                  :id (gensym)
                  :bhv bhv)))
    (push result *pending-objects*)
    result))

;; Remove a game object from the list of game objects.
(defun despawn (obj)
  (push obj *pending-despawns*))

;; Check if collision occurred to the left of an object.
(defun left-collision (obj)
  (or (collision (- (game-object-x obj) 8) (- (game-object-y obj) 5))
      (collision (- (game-object-x obj) 8) (game-object-y obj))
      (collision (- (game-object-x obj) 8) (+ (game-object-y obj) 6))))

;; Check if collision occurred to the right of an object.
(defun right-collision (obj)
  (or (collision (+ (game-object-x obj) 7) (- (game-object-y obj) 5))
      (collision (+ (game-object-x obj) 7) (game-object-y obj))
      (collision (+ (game-object-x obj) 7) (+ (game-object-y obj) 6))))

;; Check if collision occurred above an object.
(defun up-collision (obj)
  (or (collision (- (game-object-x obj) 7) (- (game-object-y obj) 7))
      (collision (game-object-x obj) (- (game-object-y obj) 7))
      (collision (+ (game-object-x obj) 6) (- (game-object-y obj) 7))))

;; Check if two objects overlap.
(defun object-collision (obj1 obj2)
  (let ((dx (abs (- (game-object-x obj1) (game-object-x obj2))))
        (dy (abs (- (mod (game-object-y obj1) (* *stage-height* 8))
                    (mod (game-object-y obj2) (* *stage-height* 8))))))
  (and (< dx 16) (< dy 16))))

;; Update all objects.
(defun update-objects ()
  ; per-object behavior
  (dolist (obj *game-objects*)
    (apply (object-bhv-update (game-object-bhv obj)) (list obj)))
  ; despawn objects if needed
  (dolist (obj *game-objects*)
    (when (game-object-other-timer obj)
      (decf (game-object-other-timer obj))
      (when (<= (game-object-other-timer obj) 0)
        (setf (game-object-other-timer obj) nil)))
    (when (> (game-object-iframes obj) 0)
      (decf (game-object-iframes obj)))
    (when (game-object-despawn-timer obj)
      (decf (game-object-despawn-timer obj))
      (when (<= (game-object-despawn-timer obj) 0)
        (despawn obj))))
  ; do all spawns
  (dolist (obj *pending-objects*)
    (push obj *game-objects*))
  (setf *pending-objects* nil)
  ; do all despawns
  (dolist (obj *pending-despawns*)
    (setf *game-objects* (delete obj *game-objects*)))
  (setf *pending-despawns* nil)
  ; common logic
  (dolist (obj *game-objects*)
    (when (game-object-has-physics obj)
      (setf (game-object-grounded obj) nil)
      ; gravity - unless bat
      (unless (eql (game-object-bhv obj) *behavior-bat*)
        (setf (game-object-yvel obj) (+ (game-object-yvel obj) *gravity*)))
      (cond ((> (game-object-yvel obj) *terminal-velocity*)
             (setf (game-object-yvel obj) *terminal-velocity*))
            ((< (game-object-yvel obj) (- *terminal-velocity*))
             (setf (game-object-yvel obj) (- *terminal-velocity*))))
      ; collision
      (setf (game-object-hit-wall obj) nil)
      (setf (game-object-x obj) (+ (game-object-x obj) (game-object-xvel obj)))
      (setf (game-object-y obj) (+ (game-object-y obj) (game-object-yvel obj)))
      (cond ((up-collision obj)
             (setf (game-object-hit-wall obj) t)
             (setf (game-object-yvel obj) 0.0)
             (setf (game-object-y obj) (float (+ (* (floor (/ (game-object-y obj) 8)) 8) 8))))
            ((or (collision (- (game-object-x obj) 7) (+ (game-object-y obj) 8))
                 (collision (game-object-x obj) (+ (game-object-y obj) 8))
                 (collision (+ (game-object-x obj) 6) (+ (game-object-y obj) 8)))
             (setf (game-object-hit-wall obj) t)
             (setf (game-object-yvel obj) 0.0)
             (setf (game-object-grounded obj) t)
             (setf (game-object-y obj) (float (* (floor (/ (game-object-y obj) 8)) 8)))))
      (cond ((left-collision obj)
             (setf (game-object-hit-wall obj) t)
             (setf (game-object-xvel obj) 0.0)
             (setf (game-object-x obj) (float (+ (* (floor (/ (game-object-x obj) 8)) 8) 8))))
            ((right-collision obj)
             (setf (game-object-hit-wall obj) t)
             (setf (game-object-xvel obj) 0.0)
             (setf (game-object-x obj) (float (* (floor (/ (game-object-x obj) 8)) 8)))))
      ; add friction if grounded
      (when (game-object-grounded obj)
        (cond ((> (game-object-xvel obj) 0.0)
               (setf (game-object-xvel obj) (- (game-object-xvel obj) 0.125))
               (when (< (game-object-xvel obj) 0.0) (setf (game-object-xvel obj) 0.0)))
              ((< (game-object-xvel obj) 0.0)
               (setf (game-object-xvel obj) (+ (game-object-xvel obj) 0.125))
               (when (> (game-object-xvel obj) 0.0) (setf (game-object-xvel obj) 0.0)))))
      (setf (game-object-anim-timer obj)
            (if (= (game-object-xvel obj) 0)
                0
                (rem (+ (game-object-anim-timer obj) (abs (/ (game-object-xvel obj) 16))) 1))))
      ; wrap vertically
    (unless (eql (game-object-bhv obj) *behavior-falling*)
      (setf (game-object-y obj) (mod (game-object-y obj) (* *stage-height* 8)))))
  ; object-object collision
  (dolist (obj1 *game-objects*)
    (let ((attack-collision nil)
          (player-collision nil)
          (ice-block-bhv nil)
          (ice-block-collision nil))
      (dolist (obj2 *game-objects*)
        (let* ((bhv1 (game-object-bhv obj1))
               (bhv2 (game-object-bhv obj2))
               (collision1 (object-bhv-collision bhv1))
               (collision2 (object-bhv-collision bhv2)))
          ; do they collide?
          (when (object-collision obj1 obj2)
            ; if so, check collision result
            (cond ((and (equal collision1 'enemy) (equal collision2 'attack) (not (equal bhv1 *behavior-dispenser*)))
                   (unless attack-collision (setq attack-collision obj2)))
                  ((and (equal collision1 'enemy) (equal collision2 'player) (= (game-object-iframes obj2) 0))
                   (unless player-collision (setq player-collision obj2)))
                  ((and (equal collision1 'enemy) (equal collision2 'ice-block) (game-object-throw obj2))
                   (unless ice-block-collision (setq ice-block-collision obj2) (setq ice-block-bhv bhv1)))))))
      ; check collisions, handle one based on priority
      (cond (ice-block-collision
              (if (equal ice-block-bhv *behavior-hardened*)
                (progn
                  (shatter-ice-block ice-block-collision)
                  (setf (game-object-other-timer obj1) 20))
                (progn
                  (add-score (game-object-throw ice-block-collision))
                  (setf (game-object-throw ice-block-collision) (* (game-object-throw ice-block-collision) 2))
                  (raylib:play-sound *sfx-destroy*)
                  (spawn-falling (game-object-x obj1) (game-object-y obj1) (object-bhv-hitsprite ice-block-bhv))
                  (despawn obj1))))
            (attack-collision
              (add-score 10)
              (turn-to-ice obj1)
              (raylib:play-sound *sfx-destroy*)
              (despawn attack-collision))
            (player-collision
              ; give the player collision a bit more leeway - 12 pixels instead of 16
              (let ((dx (abs (- (game-object-x obj1) (game-object-x player-collision))))
                    (dy (abs (- (mod (game-object-y obj1) (* *stage-height* 8))
                                (mod (game-object-y player-collision) (* *stage-height* 8))))))
                (when (and (< dx 12) (< dy 12))
                  ; put player into death state
                  (raylib:play-sound *sfx-destroy*)
                  (kill-player player-collision)))))))
  (block try-next-stage
    ; is the timer running?
    (when *stage-advance-timer*
      ; if at 0, go to next level
      (when (= *stage-advance-timer* 0)
        (setq *stage-advance-timer* nil)
        (next-stage)
        (return-from try-next-stage))
      ; decrement the timer
      (decf *stage-advance-timer*)
      (return-from try-next-stage))
    (dolist (obj *game-objects*)
      (when (equal (object-bhv-collision (game-object-bhv obj)) 'enemy)
        (return-from try-next-stage))
      (when (and (equal (object-bhv-collision (game-object-bhv obj)) 'player) (game-object-throw obj))
        (return-from try-next-stage))
      (when (equal (object-bhv-collision (game-object-bhv obj)) 'ice-block)
        (return-from try-next-stage)))
    ; no enemies left? start the advance timer
    (setf *stage-advance-timer* 180)))

;; Draw all objects.
(defun draw-objects ()
  (dolist (obj *game-objects*)
    (apply (object-bhv-draw (game-object-bhv obj)) (list obj))))
