;;;; uptime.asd

(asdf:defsystem #:uptime
  :serial t
  :version "0.0.1"
  :description "reads first and last timestamps per day from syslog"
  :author "Timo Grodzinski <timo.grodzinski@gmail.com>"
  :license "LGPLv3"
  :components ((:file "package")
               (:file "uptime"))
  :depends-on ("local-time"))

