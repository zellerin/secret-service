(defpackage secret-service-test
  (:use #:secret-service #:cl #:stefil)
  (:export #:all-tests))

(in-package secret-service-test)

(defsuite* all-tests)

(deftest standard-sessions-exist ()
  "Check that session that should always exist do."
  (dolist (col '("session" "Login"))
    (is (find-collection-by-name col))))

(defun find-all-with-id (id)
  "Find all secrets with cl-test-id being id."
  (find-all-secrets `(("cl-test-id" ,id)))  )

(defun check-ids-empty (ids)
  "Check that there are no secrets with particular ids, and offer removal."
  (dolist (id ids)
    (let ((matches (find-all-with-id id)))
      (restart-case
          (when matches
              (error "There seem to be remnants of a previous test"))
        (delete-secret () :interactively "Delete all matching secrets"
          (mapcar (lambda (a)
                    (delete-secret (car a)))
                  matches))))))

(defun delete-secrets-with-id (ids)
  (dolist (id ids)
    (mapcar (lambda (a)
              (delete-secret (car a)))
            (find-all-with-id id))))

(defmacro with-temp-secrets (ids &body body)
  "Run body after checking that there are no secrets with IDs. Delete such
 secrets when body is left."
  (let ((ids-var (gensym)))
    `(let ((,ids-var ',ids))
       (check-ids-empty ,ids-var)
       (unwind-protect
            (progn
              ,@body)
         (delete-secrets-with-id ,ids-var)))))

(deftest create-rename-delete-secret ()
  "Check that there are no secrets that would foul the tests.
Create secret, rename its label and attribute, and delete it. At each step test it works."
  (with-temp-secrets ("1" "2")
      (create-item "/org/freedesktop/secrets/collection/session" "Test secret"
                   '(("cl-test-id" "1")) "Don't tell")
    (let* ((all-matches (find-all-secrets '(("cl-test-id" "1"))))
           (match (car all-matches))
           (match-path (first match))
           (match-object (second match)))
      (is match)
      (is (null (cdr all-matches)))
      (is (equal (get-secret-item-attribute match-path "cl-test-id") "1")
          "Attribute \"1\" not correct in ~s" (get-secret-item-attributes match-path))
      (setf (get-secret-item-property match-path "Attributes") '(("cl-test-id" "2")))
      (is (equal (get-secret-item-attribute match-path "cl-test-id") "2")
          "Attribute \"2\" not correct in ~s" (get-secret-item-attributes match-path))
      (is (equal (stringify-secret match-object) "Don't tell"))
      (delete-secret match-path)
      (is (null (find-all-secrets '(("cl-test-id" "1"))))
          "The secret should not have this attribute any more")
      (is (null (find-all-secrets '(("cl-test-id" "2"))))
          "The secret should have been deleted."))))

(deftest single-finder-should-behave ()
  "Check that find-the-secret signals unless just one item found."
  (flet ((find-single-match () (find-the-secret '(("cl-test-id" "1"))))
         (add-new (label)
           (create-item "/org/freedesktop/secrets/collection/session" label
                        '(("cl-test-id" "1")) "Don't tell")))
    (with-temp-secrets ("1")
      (signals secret-item-search-error (find-single-match))
      (add-new "First")
      (is (find-single-match))
      (is (equal (find-single-match) "Don't tell"))
      (add-new "Second")
      (signals secret-item-search-error (find-single-match)))))
