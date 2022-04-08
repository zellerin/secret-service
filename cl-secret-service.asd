;;;; cl-secret-service.asd

(asdf:defsystem #:cl-secret-service
  :description "Interface to Secret Service API"
  :author "Tomáš Zellerin <tomas@zellerin.cz>"
  :license  "Specify license here"
  :version "0.1"
  :serial t
  :depends-on ("dbus")
  :components ((:file "package")
               (:file "cl-secret-service")))
