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
              (px (* x 8))
              (py (* y 8)))
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
                                                    (3d-vectors:vec px py)
                                                    raylib:+white+))))))
  (eval (stage-desc-enemies *current-stage*)))

(defun load-stage ()
  (reset-objects)
  (spawn-player)
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
