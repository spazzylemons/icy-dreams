(in-package :icy-dreams)

;; The list of game objects.
(defparameter *game-objects* nil)

;; The list of game objects pending for spawning.
(defparameter *pending-objects* nil)

;; The list of game objects pending for despawning.
(defparameter *pending-despawns* nil)

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
                  :throw nil
                  :hit-wall nil
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

;; Check if two objects overlap.
(defun object-collision (obj1 obj2)
  (let ((dx (abs (- (game-object-x obj1) (game-object-x obj2))))
        (dy (abs (- (mod (game-object-y obj1) (* *stage-height* 8))
                    (mod (game-object-y obj2) (* *stage-height* 8))))))
  (and (<= dx 16) (<= dy 16))))

;; Update all objects.
(defun update-objects ()
  ; per-object behavior
  (dolist (obj *game-objects*)
    (apply (object-bhv-update (game-object-bhv obj)) (list obj)))
  ; despawn objects if needed
  (dolist (obj *game-objects*)
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
      ; gravity
      (setf (game-object-yvel obj) (+ (game-object-yvel obj) *gravity*))
      (cond ((> (game-object-yvel obj) *terminal-velocity*)
             (setf (game-object-yvel obj) *terminal-velocity*))
            ((< (game-object-yvel obj) (- *terminal-velocity*))
             (setf (game-object-yvel obj) (- *terminal-velocity*))))
      ; collision
      (setf (game-object-hit-wall obj) nil)
      (setf (game-object-x obj) (+ (game-object-x obj) (game-object-xvel obj)))
      (setf (game-object-y obj) (+ (game-object-y obj) (game-object-yvel obj)))
      (cond ((or (collision (- (game-object-x obj) 7) (- (game-object-y obj) 7))
                 (collision (game-object-x obj) (- (game-object-y obj) 7))
                 (collision (+ (game-object-x obj) 6) (- (game-object-y obj) 7)))
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
      (setf (game-object-y obj) (mod (game-object-y obj) (* *stage-height* 8))))
  ; object-object collision
  (dolist (obj1 *game-objects*)
    (dolist (obj2 *game-objects*)
      (let ((collision1 (object-bhv-collision (game-object-bhv obj1)))
            (collision2 (object-bhv-collision (game-object-bhv obj2))))
        ; do they collide?
        (when (object-collision obj1 obj2)
          ; if so, check collision result
          (cond ((and (equal collision1 'enemy) (equal collision2 'attack))
                 ; attack turns enemy into ice, destroys attack object
                 (turn-to-ice obj1)
                 (despawn obj2))
                ((and (equal collision1 'enemy) (equal collision2 'ice-block))
                  ; if the ice is being thrown, destroy the other enemy
                  ; TODO animation for destroyed enemy
                  (when (game-object-throw obj2) (despawn obj1)))))))))

;; Draw all objects.
(defun draw-objects ()
  (dolist (obj *game-objects*)
    (apply (object-bhv-draw (game-object-bhv obj)) (list obj))))
