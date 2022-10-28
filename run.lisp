(require "asdf")
(asdf:load-asd (merge-pathnames "icy-dreams.asd" (uiop:getcwd)))
(asdf:load-system :icy-dreams)
