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
  (make-stage-desc :tile 0)
  (make-stage-desc :tile 0)
  (make-stage-desc :tile 0)
  (make-stage-desc :tile 0)
  (make-stage-desc :tile 0)
  (make-stage-desc :tile 0)
  (make-stage-desc :tile 0)
  (make-stage-desc :tile 0)
  (make-stage-desc :tile 0)
  (make-stage-desc :tile 0)
  (make-stage-desc :tile 0)
  (make-stage-desc :tile 0)
  (make-stage-desc :tile 0)
  (make-stage-desc :tile 0)
  (make-stage-desc :tile 0)
  (make-stage-desc :tile 0)
  (make-stage-desc :tile 0)
  (make-stage-desc :tile 0)
  (make-stage-desc :tile 0)
  (make-stage-desc :tile 0)
  (make-stage-desc :tile 0)
  (make-stage-desc :tile 0)
  (make-stage-desc :tile 0)
  (make-stage-desc :tile 0)
  (make-stage-desc :tile 0)
  (make-stage-desc :tile 0)
  (make-stage-desc :tile 0)))

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
  (raylib:unload-image image))
