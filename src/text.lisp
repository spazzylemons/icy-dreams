(in-package :icy-dreams)

(defvar *font*)

(defun load-font ()
  (setf *font* (raylib:load-font "assets/font.png")))

(defun printf (x y fmt &rest args)
  (raylib:draw-text-ex *font*
                       (apply #'format `(nil ,fmt . ,args))
                       (3d-vectors:vec (float x) (float y))
                       7.0
                       1.0
                       raylib:+white+))
