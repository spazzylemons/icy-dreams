(in-package :icy-dreams)

;; The list of game objects.
(defparameter *game-objects* nil)

;; Spawn a game object. Returns the object.
(defun spawn (bhv)
  (let ((result (make-game-object
                  :x 0.0
                  :y 0.0
                  :xvel 0.0
                  :yvel 0.0
                  :id (gensym)
                  :bhv bhv)))
    (push result *game-objects*)
    result))

;; Remove a game object from the list of game objects.
(defun despawn (obj)
  (delete obj *game-objects*))

;; Update all objects.
(defun update-objects ()
  (dolist (obj *game-objects*)
    (apply (object-bhv-update (game-object-bhv obj)) (list obj)))
  ; apply velocity + collision in its own phase
  (dolist (obj *game-objects*)
    (setf (game-object-x obj) (+ (game-object-x obj) (game-object-xvel obj)))
    (setf (game-object-y obj) (+ (game-object-y obj) (game-object-yvel obj)))
    (cond ((or (collision (- (game-object-x obj) 7) (- (game-object-y obj) 7))
               (collision (game-object-x obj) (- (game-object-y obj) 7))
               (collision (+ (game-object-x obj) 6) (- (game-object-y obj) 7)))
           (setf (game-object-yvel obj) 0.0)
           (setf (game-object-y obj) (float (+ (* (floor (/ (game-object-y obj) 8)) 8) 8))))
          ((or (collision (- (game-object-x obj) 7) (+ (game-object-y obj) 7))
               (collision (game-object-x obj) (+ (game-object-y obj) 7))
               (collision (+ (game-object-x obj) 6) (+ (game-object-y obj) 7)))
           (setf (game-object-yvel obj) 0.0)
           (setf (game-object-y obj) (float (* (floor (/ (game-object-y obj) 8)) 8)))))
    (cond ((or (collision (- (game-object-x obj) 8) (- (game-object-y obj) 5))
               (collision (- (game-object-x obj) 8) (+ (game-object-y obj) 6)))
           (setf (game-object-xvel obj) 0.0)
           (setf (game-object-x obj) (float (+ (* (floor (/ (game-object-x obj) 8)) 8) 8))))
          ((or (collision (+ (game-object-x obj) 7) (- (game-object-y obj) 5))
               (collision (+ (game-object-x obj) 7) (+ (game-object-y obj) 6)))
           (setf (game-object-xvel obj) 0.0)
           (setf (game-object-x obj) (float (* (floor (/ (game-object-x obj) 8)) 8)))))))

;; Draw all objects.
(defun draw-objects ()
  (dolist (obj *game-objects*)
    (apply (object-bhv-draw (game-object-bhv obj)) (list obj))))
