(ql:quickload "cl-raylib")

(defparameter *screen-width* 256)
(defparameter *screen-height* 224)

;; game objects
(defstruct game-object x y id)

(defparameter *game-objects* nil)

; temporary
(defvar *test-object*)

(defun spawn ()
  (let ((result (make-game-object :x 0.0 :y 0.0 :id (gensym))))
    (push result *game-objects*)
    result))

(defun despawn (obj)
  (delete obj *game-objects*))

(defun update-game ()
  (if (raylib:is-key-down raylib:+key-left+)
    (decf (game-object-x *test-object*)))
  (if (raylib:is-key-down raylib:+key-right+)
    (incf (game-object-x *test-object*)))
  (if (raylib:is-key-down raylib:+key-up+)
    (decf (game-object-y *test-object*)))
  (if (raylib:is-key-down raylib:+key-down+)
    (incf (game-object-y *test-object*))))

(defvar *spritesheet*)

(defun draw-sprite (x y id flipped)
  (let ((src (raylib:make-rectangle :x 0.0
                                    :y (* id 16.0)
                                    :width (if flipped -16.0 16.0)
                                    :height 16.0)))
    (raylib:draw-texture-rec *spritesheet*
                             src
                             (3d-vectors:vec (round x) (round y))
                             raylib:+white+)))

(defun render-game ()
  (raylib:clear-background raylib:+white+)
  (raylib:draw-text "hello world!" 10 10 20 raylib:+red+)
  (dolist (obj *game-objects*)
    (draw-sprite (game-object-x obj) (game-object-y obj) 0 nil)))

(defun main-loop (target-texture)
  (loop
    (if (raylib:window-should-close) (return))
    (update-game)
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

(let ((obj-a (spawn))
      (obj-b (spawn)))
  (setf (game-object-x obj-a) 10.0)
  (setf (game-object-y obj-a) 10.0)
  (setf *test-object* obj-a)
  (setf (game-object-x obj-b) 40.0)
  (setf (game-object-y obj-b) 40.0))

(raylib:set-config-flags '(:flag-window-resizable :flag-vsync-hint))
(raylib:with-window ((* *screen-width* 2) (* *screen-height* 2) "Icy Dreams")
  (setf *spritesheet* (raylib:load-texture "assets/spritesheet.png"))
  (raylib:set-window-min-size *screen-width* *screen-height*)
  (let ((target-texture (raylib:load-render-texture *screen-width* *screen-height*)))
    (unwind-protect (main-loop target-texture))
    (raylib:unload-render-texture target-texture)))
