(in-package :icy-dreams)

(defun update-game ()
  (if *level-transition-timer*
    (when (= *level-transition-timer* 0)
      (prerender-stage))
    (update-objects)))

(defun render-game ()
  (raylib:clear-background raylib:+black+)
  (draw-stage)
  (unless *level-transition-timer*
    (draw-objects))
  (draw-transition)
  ; draw black bars to mask object wraparound
  (raylib:draw-rectangle 0 0 *screen-width* 16 raylib:+black+)
  (raylib:draw-rectangle 0 (- *screen-height* 16) *screen-width* 16 raylib:+black+))

(defun main-loop-with-music (target-texture music)
  (raylib:play-music-stream music)
  (loop
    (if (raylib:window-should-close) (return))
    (raylib:update-music-stream music)
    (handler-case (update-game)
      (game-complete ()
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

(raylib:set-config-flags '(:flag-window-resizable :flag-vsync-hint))
(raylib:set-target-fps 60)
(raylib:with-window ((* *screen-width* 2) (* *screen-height* 2) "Icy Dreams")
  (load-spritesheet)
  (load-stage)

  ; uncomment to skip stages
  (dotimes (n 18)
    (next-stage))
  (raylib:with-audio-device
    (raylib:set-window-min-size *screen-width* *screen-height*)
    (let ((target-texture (raylib:load-render-texture *screen-width* *screen-height*)))
      (unwind-protect (main-loop target-texture))
      (raylib:unload-render-texture target-texture))))
