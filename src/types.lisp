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
