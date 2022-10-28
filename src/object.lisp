(in-package :icy-dreams)

;; The list of game objects.
(defparameter *game-objects* nil)

;; Spawn a game object. Returns the object.
(defun spawn (bhv)
  (let ((result (make-game-object :x 0.0 :y 0.0 :id (gensym) :bhv bhv)))
    (push result *game-objects*)
    result))

;; Remove a game object from the list of game objects.
(defun despawn (obj)
  (delete obj *game-objects*))

;; Update all objects.
(defun update-objects ()
  (dolist (obj *game-objects*)
    (apply (object-bhv-update (game-object-bhv obj)) (list obj))))

;; Draw all objects.
(defun draw-objects ()
  (dolist (obj *game-objects*)
    (apply (object-bhv-draw (game-object-bhv obj)) (list obj))))
