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

(defparameter *stage-palette* (make-hash-table))

(setf (gethash (raylib::make-rgba 0 0 0 255) *stage-palette*) 0)
(setf (gethash (raylib::make-rgba 255 255 255 255) *stage-palette*) 1)
(setf (gethash (raylib::make-rgba 255 0 0 255) *stage-palette*) 2)
(setf (gethash (raylib::make-rgba 0 255 0 255) *stage-palette*) 3)
(setf (gethash (raylib::make-rgba 0 0 255 255) *stage-palette*) 4)
(setf (gethash (raylib::make-rgba 255 0 255 255) *stage-palette*) 5)
(setf (gethash (raylib::make-rgba 255 255 0 255) *stage-palette*) 6)

(defparameter *stages* (list
  (make-stage-desc :tile 0)
  (make-stage-desc :tile 1)
  (make-stage-desc :tile 2)
  (make-stage-desc :tile 3)
  (make-stage-desc :tile 4)
  (make-stage-desc :tile 0)
  (make-stage-desc :tile 1)
  (make-stage-desc :tile 4)
  (make-stage-desc :tile 3)
  (make-stage-desc :tile 2)
  (make-stage-desc :tile 0)
  (make-stage-desc :tile 1)
  (make-stage-desc :tile 4)
  (make-stage-desc :tile 2)
  (make-stage-desc :tile 3)
  (make-stage-desc :tile 1)
  (make-stage-desc :tile 3)
  (make-stage-desc :tile 2)
  (make-stage-desc :tile 4)
  (make-stage-desc :tile 0)
  (make-stage-desc :tile 1)
  (make-stage-desc :tile 4)
  (make-stage-desc :tile 0)
  (make-stage-desc :tile 3)
  (make-stage-desc :tile 2)
  (make-stage-desc :tile 1)
  (make-stage-desc :tile 0)
  (make-stage-desc :tile 4)
  (make-stage-desc :tile 2)
  (make-stage-desc :tile 3)))

(defun read-stages-from-assets ()
  (let ((image (raylib:load-image "assets/stages.png")))
    (unwind-protect (let ((base-y 0))
                      (dolist (stage *stages*)
                        (let ((tilemap (make-array `(,*stage-height* ,*stage-width*))))
                          (dotimes (y *stage-height*)
                            (dotimes (x *stage-width*)
                              (let ((color (cl-raylib::get-image-color image x (+ y base-y))))
                                (setf (aref tilemap y x) (gethash color *stage-palette*)))))
                          (setf (stage-desc-tilemap stage) tilemap))
                          (setf base-y (+ base-y *stage-height*)))))
    (raylib:unload-image image)))
