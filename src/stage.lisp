(in-package :icy-dreams)

(defparameter *current-stage* (car *stages*))

;; Takes in a position, not a tile grid index.
(defun collision (x y)
  (let ((tx (round (/ x 8)))
        (ty (round (/ y 8))))
    (cond ((< tx 0) (setq tx 0))
          ((>= tx *stage-width*) (setq tx (1- *stage-width*))))
    (cond ((< ty 0) (setq ty 0))
          ((>= ty *stage-height*) (setq ty (1- *stage-height*))))
    (= 1 (aref (stage-desc-tilemap *current-stage*) ty tx))))
