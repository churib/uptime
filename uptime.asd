;;;; uptime.asd

(asdf:defsystem #:uptime
  :serial t
  :description "Describe uptime here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :components ((:file "package")
               (:file "uptime"))
  :depends-on ("chronicity" "simple-date-time"))

