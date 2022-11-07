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

(defparameter *current-stage* (car *stages*))
(defparameter *next-stages* (cdr *stages*))

(defparameter *stage-texture* nil)
(defparameter *tile-texture* nil)

(defparameter *level-transition-timer* nil)

;; Takes in a position, not a tile grid index.
(defun collision (x y)
  (let ((tx (floor (/ x 8)))
        (ty (mod (floor (/ y 8)) *stage-height*)))
    (if (or (< tx 0)
            (>= tx *stage-width*))
      t
      (= 1 (aref (stage-desc-tilemap *current-stage*) ty tx)))))

(defun next-stage ()
  ; TODO endgame stuff
  (unless *next-stages*
    (signal 'game-complete))
  (setf *current-stage* (car *next-stages*))
  (setf *next-stages* (cdr *next-stages*))
  (load-stage))

(defun prerender-stage ()
  (raylib:with-texture-mode (*stage-texture*)
    (raylib:clear-background raylib:+blank+)
    (dotimes (y *stage-height*)
      (dotimes (x *stage-width*)
        (let ((adjacent 0)
              (px (+ (* x 8) 4))
              (py (+ (* y 8) 4))
              (tile-here (aref (stage-desc-tilemap *current-stage*) y x)))
          (cond ((= tile-here 2) (spawn-roller px py))
                ((= tile-here 3) (spawn-springy px py))
                ((= tile-here 4) (spawn-hardened px py))
                ((= tile-here 5) (spawn-bat px py))
                ((= tile-here 6) (spawn-dispenser px py)))
          (when (collision px py)
            (if (collision px (- py 8)) (setq adjacent (logior adjacent 1)))
            (if (collision (+ px 8) py) (setq adjacent (logior adjacent 2)))
            (if (collision px (+ py 8)) (setq adjacent (logior adjacent 4)))
            (if (collision (- px 8) py) (setq adjacent (logior adjacent 8)))
            (raylib:draw-texture-rec *tile-texture* (raylib:make-rectangle
                                                      :x (* adjacent 8)
                                                      :y (* (stage-desc-tile *current-stage*) 8)
                                                      :width 8
                                                      :height 8)
                                                    (3d-vectors:vec (- px 4) (- py 4))
                                                    raylib:+white+)))))))

(defun load-stage ()
  (reset-objects)
  (spawn-player 0)
  (setf *level-transition-timer* 8)
  (unless *stage-texture*
    (setq *stage-texture* (raylib:load-render-texture (* *stage-width* 8) (* *stage-height* 8))))
  (unless *tile-texture*
    (setq *tile-texture* (raylib:load-texture "assets/tiles.png"))))

(defun draw-stage ()
  (let ((texture (raylib:render-texture-texture *stage-texture*)))
    (raylib:draw-texture-pro texture
                             (raylib:make-rectangle
                               :x 0.0
                               :y 0.0
                               :width (raylib:texture-width texture) 
                               :height (- (raylib:texture-height texture)))
                             (raylib:make-rectangle
                               :x 0.0
                               :y 16.0
                               :width (raylib:texture-width texture) 
                               :height (raylib:texture-height texture))
                             (3d-vectors:vec 0.0 0.0)
                             0.0
                             raylib:+white+)))

(defun draw-transition ()
  (when *level-transition-timer*
    (dotimes (y *stage-height*)
      (raylib:draw-rectangle 0 (+ (* y 8) 16) *screen-width* (- 8 (abs *level-transition-timer*)) raylib:+black+))
    (decf *level-transition-timer*)
    (when (= *level-transition-timer* -9)
      (setf *level-transition-timer* nil))))
