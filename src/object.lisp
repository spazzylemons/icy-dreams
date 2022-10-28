(in-package :icy-dreams)

;; The list of game objects.
(defparameter *game-objects* nil)

;; The update routines for objects.
(defparameter *update-routines* (make-hash-table))

;; The draw routines for objects.
(defparameter *draw-routines* (make-hash-table))

;; Fill the hash tables.
(setf (gethash 'player *update-routines*) #'update-player)
(setf (gethash 'player *draw-routines*) #'draw-player)

;; Spawn a game object. Returns the object.
(defun spawn (kind)
  (let ((result (make-game-object :x 0.0 :y 0.0 :id (gensym) :kind kind)))
    (push result *game-objects*)
    result))

;; Remove a game object from the list of game objects.
(defun despawn (obj)
  (delete obj *game-objects*))

;; Update all objects.
(defun update-objects ()
  (dolist (obj *game-objects*)
    (apply (gethash (game-object-kind obj) *update-routines*) (list obj))))

;; Draw all objects.
(defun draw-objects ()
  (dolist (obj *game-objects*)
    (apply (gethash (game-object-kind obj) *draw-routines*) (list obj))))
