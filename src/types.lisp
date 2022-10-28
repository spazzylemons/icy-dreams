(in-package :icy-dreams)

;; The game object type.
(defstruct game-object x y id bhv)

;; Behavior for an object.
(defstruct object-bhv update draw)
