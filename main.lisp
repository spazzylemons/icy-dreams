(ql:quickload "cl-raylib")

(defparameter *screen-width* 256)
(defparameter *screen-height* 224)

(defun render-game ()
  (raylib:clear-background raylib:+white+)
  (raylib:draw-text "hello world!" 10 10 20 raylib:+red+))

(defun main-loop (target-texture)
  (loop
    (if (raylib:window-should-close) (return))
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

(raylib:set-config-flags '(:flag-window-resizable :flag-vsync-hint))
(raylib:with-window ((* *screen-width* 2) (* *screen-height* 2) "Icy Dreams")
  (raylib:set-window-min-size *screen-width* *screen-height*)
  (let ((target-texture (raylib:load-render-texture *screen-width* *screen-height*)))
    (unwind-protect (main-loop target-texture))
    (raylib:unload-render-texture target-texture)))
