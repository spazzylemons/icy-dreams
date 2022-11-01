(in-package :icy-dreams)

;; The game object type.
(defstruct game-object x y xvel yvel grounded has-physics anim-timer direction despawn-timer other-timer throw hit-wall id bhv)

;; Behavior for an object.
(defstruct object-bhv update draw collision)

;; A stage description.
(defstruct stage-desc tile tilemap)

;; The condition in which a game is completed.
(define-condition game-complete () ())
