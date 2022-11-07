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

(defparameter *screen-width* 256)
(defparameter *screen-height* 224)

;; The width of a stage, in tiles.
(defparameter *stage-width* 32)

;; The height of a stage, in tiles.
(defparameter *stage-height* 24)

;; The maximum velocity.
(defparameter *terminal-velocity* 2.875)

;; The speed of gravity.
(defparameter *gravity* 0.09375)

; not constants but whatever the jam's gonna end soon

(defparameter *sfx-jump* nil)

(defparameter *sfx-attack* nil)

(defparameter *sfx-destroy* nil)
