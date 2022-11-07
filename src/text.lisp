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

(defvar *font*)

(defun load-font ()
  (setf *font* (raylib:load-font "assets/font.png")))

(defun printf (x y fmt &rest args)
  (raylib:draw-text-ex *font*
                       (apply #'format `(nil ,fmt . ,args))
                       (3d-vectors:vec (float x) (float y))
                       7.0
                       1.0
                       raylib:+white+))
