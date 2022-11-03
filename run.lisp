; load via quicklisp just to make sure the packages are installed
(ql:quickload :cl-raylib)
; load and run the system
(require "asdf")
(asdf:load-asd (merge-pathnames "icy-dreams.asd" (uiop:getcwd)))
(asdf:load-system :icy-dreams)
