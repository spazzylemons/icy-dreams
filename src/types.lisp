(in-package :icy-dreams)

;; The game object type.
(defstruct game-object x y xvel yvel grounded has-physics anim-timer direction despawn-timer other-timer throw hit-wall iframes dead id bhv)

;; Behavior for an object.
(defstruct object-bhv update draw collision hitsprite)

;; A stage description.
(defstruct stage-desc tile tilemap)

;; The condition in which a game is completed.
(define-condition game-complete () ())

;; The condition in which a game is lost.
(define-condition game-lost () ())

;; The condition in which the program should exit.
(define-condition game-exit () ())
