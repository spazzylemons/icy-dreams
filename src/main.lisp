(in-package :icy-dreams)

(defparameter *game-paused* nil)

(defparameter *title-texture* nil)
(defparameter *gameover-texture* nil)
(defparameter *gamewin-texture* nil)

(defparameter *fullscreen-texture* nil)

(defun update-game (music)
  (if *fullscreen-texture*
    (when (raylib:is-key-pressed raylib:+key-enter+)
      (unless (equal *fullscreen-texture* *title-texture*)
        (signal 'game-exit))
      (setf *fullscreen-texture* nil)))
  (unless *fullscreen-texture*
    (when (raylib:is-key-pressed raylib:+key-p+)
      (setf *game-paused* (not *game-paused*))
      (if *game-paused* (raylib:pause-music-stream music) (raylib:play-music-stream music)))
    (unless *game-paused*
      (if *level-transition-timer*
        (when (= *level-transition-timer* 0)
          (prerender-stage))
        (update-objects)))))

(defun render-game ()
  (if *fullscreen-texture*
    (progn
      (raylib:draw-texture *fullscreen-texture* 0 0 raylib:+white+)
      (unless (equal *fullscreen-texture* *title-texture*)
        (printf (- 72 (* 4 (length (format nil "~a" *current-score*)))) 152 "YOUR SCORE IS ~a" *current-score*)))
    (progn
      (raylib:clear-background raylib:+black+)
      (draw-stage)
      (unless *level-transition-timer*
        (draw-objects))
      (draw-transition)
      ; draw black bars to mask object wraparound
      (raylib:draw-rectangle 0 0 *screen-width* 16 raylib:+black+)
      (raylib:draw-rectangle 0 (- *screen-height* 16) *screen-width* 16 raylib:+black+)
      (draw-score)
      (printf 8 0 "LIVES ~a" *num-lives*)
      (when *game-paused*
        (printf 108 124 "PAUSE")))))

(defun main-loop-with-music (target-texture music)
  (raylib:play-music-stream music)
  (loop
    (if (raylib:window-should-close) (return))
    (raylib:update-music-stream music)
    (handler-case (update-game music)
      (game-complete ()
        (setf *fullscreen-texture* *gamewin-texture*))
      (game-lost ()
        (setf *fullscreen-texture* *gameover-texture*))
      (game-exit ()
        (return)))
    (let* ((scale (min (/ (float (raylib:get-screen-width)) *screen-width*)
                       (/ (float (raylib:get-screen-height)) *screen-height*)))
           (width-scale (* scale *screen-width*))
           (height-scale (* scale *screen-height*))
           (texture (raylib:render-texture-texture target-texture))
           (src-rect (raylib:make-rectangle :x 0.0
                                            :y 0.0
                                            :width (raylib:texture-width texture)
                                            :height (- (raylib:texture-height texture))))
           (dst-rect (raylib:make-rectangle :x (* (- (raylib:get-screen-width) width-scale) 0.5)
                                            :y (* (- (raylib:get-screen-height) height-scale) 0.5)
                                            :width width-scale
                                            :height height-scale)))
      (raylib:with-texture-mode (target-texture)
        (render-game))
      (raylib:with-drawing
        (raylib:clear-background raylib:+black+)
        (raylib:draw-texture-pro texture src-rect dst-rect (3d-vectors:vec 0.0 0.0) 0.0 raylib:+white+)))))

(defun main-loop (target-texture)
  (let ((music (raylib:load-music-stream "assets/theme.ogg")))
    (unwind-protect (main-loop-with-music target-texture music))
    (raylib:unload-music-stream music)))

(defun buildapp-main ()
  (raylib:set-config-flags '(:flag-window-resizable :flag-vsync-hint))
  (raylib:set-target-fps 60)
  (raylib:with-window ((* *screen-width* 2) (* *screen-height* 2) "Icy Dreams")
    (read-stages-from-assets)
    (load-font)
    (load-spritesheet)
    (load-stage)
    (setf *title-texture* (raylib:load-texture "assets/title.png"))
    (setf *gameover-texture* (raylib:load-texture "assets/gameover.png"))
    (setf *gamewin-texture* (raylib:load-texture "assets/gamewin.png"))
    (setf *fullscreen-texture* *title-texture*)
    ; uncomment to skip stages
    ; (dotimes (n 29)
    ;   (next-stage))
    (raylib:with-audio-device
      (raylib:set-window-min-size *screen-width* *screen-height*)
      (let ((target-texture (raylib:load-render-texture *screen-width* *screen-height*)))
        (unwind-protect (main-loop target-texture))
        (raylib:unload-render-texture target-texture)))))
