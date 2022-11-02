(in-package :icy-dreams)

(defparameter *stage-palette* (make-hash-table :test 'equal))

(setf (gethash '(0 0 0) *stage-palette*) 0)
(setf (gethash '(255 255 255) *stage-palette*) 1)
(setf (gethash '(255 0 0) *stage-palette*) 2)
(setf (gethash '(0 255 0) *stage-palette*) 3)
(setf (gethash '(0 0 255) *stage-palette*) 4)
(setf (gethash '(255 0 255) *stage-palette*) 5)

(defparameter *stages* (list
  (make-stage-desc :tile 0)
  (make-stage-desc :tile 1)
  (make-stage-desc :tile 2)
  (make-stage-desc :tile 0)
  (make-stage-desc :tile 0)
  (make-stage-desc :tile 0)))

(let ((image (with-open-file (f #p"assets/stages.png"
                                :element-type '(unsigned-byte 8))
               (png:decode f)))
      (base-y 0))
  (dolist (stage *stages*)
    (let ((tilemap (make-array `(,*stage-height* ,*stage-width*))))
      (dotimes (y *stage-height*)
        (dotimes (x *stage-width*)
          (let ((r (aref image (+ y base-y) x 0))
                (g (aref image (+ y base-y) x 1))
                (b (aref image (+ y base-y) x 2)))
            (setf (aref tilemap y x) (gethash `(,r ,g ,b) *stage-palette*)))))
      (setf (stage-desc-tilemap stage) tilemap))
      (setf base-y (+ base-y *stage-height*))))