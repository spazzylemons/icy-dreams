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
               (:file "text")
               (:file "score")
               (:file "stages")
               (:file "stage")
               (:file "sprite")
               (:file "object")
               (:file "object/ice-cloud")
               (:file "object/ice-shard")
               (:file "object/ice-block")
               (:file "object/falling")
               (:file "object/player")
               (:file "object/roller")
               (:file "object/springy")
               (:file "object/hardened")
               (:file "object/bat")
               (:file "object/dispenser")
               (:file "main")))
