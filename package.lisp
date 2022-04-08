(defpackage #:cl-secret-service
  (:use #:cl #:dbus)
  (:export #:find-all-secrets #:stringify-secret #:get-secret-item-attributes #:set-secret-item-attributes))
