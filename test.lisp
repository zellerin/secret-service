(defpackage cl-secret-service-test
  (:use #:cl-secret-service #:cl #:stefil))

(in-package cl-secret-service-test)

(defsuite* all-tests)

(deftest standard-sessions-exist ()
  (is (find "/login" (get-collections-list) :test 'search))
  (is (find "/session" (get-collections-list) :test 'search)))


(deftest create-rename-delete-secret ()
  "Check that there are no secrets that would foul the tests.
Create secret, rename its label and attribute, and delete it. At each step test it works."
  (restart-case
      (is (null (find-all-secrets '("cl-test-id" "1")))
          "There seem to be remnants of a previous test")
    (delete-secret () :report-expression "Delete all matching secrets"
      (mapcar (lambda (a)
                (delete-secret (car a)))
              (find-all-secrets '("cl-test-id" "1")))))

  (restart-case
      (is (null (find-all-secrets '("cl-test-id" "2")))
          "There seem to be remnants of a previous test")
    (delete-secret () :report-expression "Delete all matching secrets"
      (mapcar (lambda (a)
                (delete-secret (car a)))
              (find-all-secrets '("cl-test-id" "1")))))
  (create-item "/org/freedesktop/secrets/collection/session" "Test secret"
               '(("cl-test-id" "1")) "Don't tell")
  (unwind-protect
       (let* ((all-matches (find-all-secrets '("cl-test-id" "1")))
              (match (car all-matches))
              (match-path (car match)))
         (is match)
         (is (null (cdr all-matches)))
         (is (equal (get-secret-item-attribute match-path "cl-test-id") "1")
             "Attribute not correct in ~s" (get-secret-item-attributes match-path))
         (setf (get-secret-item-property match-path "Attributes") '(("cl-test-id" "2")))
         (is (equal (get-secret-item-attribute match-path "cl-test-id") "2")
             "Attribute not correct in ~s" (get-secret-item-attributes match-path))
         (is (equal (stringify-secret match) "Don't tell"))
         (delete-secret match-path)
         (is (null (find-all-secrets '("cl-test-id" "1")))
             "The secret should not have this attribute any more")
         (is (null (find-all-secrets '("cl-test-id" "2")))
             "The secret should have been deleted."))
    (mapcar (lambda (a)
              (delete-secret (car a)))
            (find-all-secrets '("cl-test-id" "1")))
    (mapcar (lambda (a)
              (delete-secret (car a)))
            (find-all-secrets '("cl-test-id" "2")))))
