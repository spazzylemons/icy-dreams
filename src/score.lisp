(in-package :icy-dreams)

(defparameter *current-score* 0)

(defun add-score (value)
  (setf *current-score* (+ *current-score* value)))

; TODO use custom font
(defun draw-score ()
  (printf 8 8 "SCORE ~a" *current-score*))
