(in-package :icy-dreams)

;; The game object type.
(defstruct game-object x y xvel yvel id bhv)

;; Behavior for an object.
(defstruct object-bhv update draw)

;; A stage description.
(defstruct stage-desc tile tilemap)
