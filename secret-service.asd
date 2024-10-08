;;;; secret-service.asd
(in-package :asdf-user)

(asdf:defsystem #:secret-service
  :description "Interface to Secret Service API"
  :author "Tomáš Zellerin <tomas@zellerin.cz>"
  :license  "Specify license here"
  :version "1.0"
  :serial t
  :homepage "https://doc.zellerin.cz/secret-service/"
  :source-control (:git "https://github.com/zellerin/secret-service")
  :depends-on ("dbus" "mgl-pax")
  :components ((:file "package")
               (:file "secret-service"))
  :in-order-to ((test-op (test-op "secret-service/test"))))

(asdf:defsystem #:secret-service/test
  :description "Tests for the interface to Secret Service API"
  :author "Tomáš Zellerin <tomas@zellerin.cz>"
  :license  "Specify license here"
  :version "1.0"
  :serial t
  :depends-on ("secret-service" "stefil")
  :components ((:file "test"))
  :perform (test-op (o s)
                    (uiop:symbol-call "secret-service-test" '#:all-tests)))
