(defpackage #:cl-secret-service
  (:use #:cl #:dbus)
  (:export #:find-all-secrets #:stringify-secret #:get-secret-item-attributes
           #:get-secret-item-attribute
           #:get-secret-item-property
           #:get-collections-list
           #:create-item #:delete-secret))
