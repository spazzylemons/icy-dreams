; load the system
(require "asdf")
(asdf:load-asd (merge-pathnames "icy-dreams.asd" (uiop:getcwd)))
(asdf:load-system :icy-dreams)
; create executable
(sb-ext:save-lisp-and-die "icy-dreams.exe"
  :toplevel #'icy-dreams:buildapp-main
  :application-type :gui
  :compression 9
  :executable t)
