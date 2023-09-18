(mgl-pax:define-package #:secret-service
  (:use #:cl #:dbus #:mgl-pax)
  (:export #:find-all-secrets #:stringify-secret #:get-secret-item-attributes
           #:get-secret-item-attribute
           #:get-secret-item-property
           #:secret-item-search-error
           #:get-collections-list
           #:find-collection-by-name
           #:get-collection-by-alias
           #:get-collection-attributes
           #:find-the-secret
           #:create-item #:delete-secret))
