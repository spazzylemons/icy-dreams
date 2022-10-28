(asdf:defsystem :icy-dreams
  :description "Arcade platformer game for Lisp Game Jam 2022"
  :version "0.1.0"
  :author "spazzylemons"
  :license "GPL3"
  :depends-on (:cl-raylib)
  :pathname "src"
  :serial t
  :components ((:file "package")
               (:file "constants")
               (:file "types")
               (:file "stages")
               (:file "stage")
               (:file "sprite")
               (:file "object/player")
               (:file "object")
               (:file "main")))
