;;;; tetris.asd

(asdf:defsystem #:tetris
  :description "Describe tetris here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :depends-on (:cl-charms :alexandria)
  :components ((:file "package")
	       (:file "console")
               (:file "tetris")))

