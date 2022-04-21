(in-package #:cl-secret-service)

;;;; D-Bus primer:
;;;; - path and secrets-service name give you an object.
;;;; - Object, its "type"/interface name (object has several and can be probed for)  and method name (again, part of objects hash tables) give you a function to call. And the function  needs parameters.
;;;; - Objects have also attributes/properties, and a separate interface to work with them.

(defvar secrets-service "org.freedesktop.secrets"
  "The D-Bus name used to talk to Secret Service.")

(defvar secrets-path "/org/freedesktop/secrets"
  "The D-Bus root object path used to talk to Secret Service.")

(defvar secrets-session-collection-path
  "/org/freedesktop/secrets/collection/session"
  "The D-Bus temporary session collection object path.")

(defvar secrets-interface-item "org.freedesktop.Secret.Item"
  "A collection of items containing secrets.")

(defun dbus-call-method (path service2 &rest args)
  "Simple single Secret service call."
  (with-open-bus (bus (session-server-addresses))
    (with-introspected-object (ss bus path secrets-service)
      (apply #'ss service2 args))))


;;;; Sessions
(defun funcall-with-session (fn)
  "Helper for macro below.

The only purpose of sessions here is to provide information about how password should be protected on fly. We do not protect it now (PLAIN method)."
  (with-open-bus (bus (session-server-addresses))
     (with-introspected-object (ss bus secrets-path secrets-service)
       (let ((session (nth-value 1 (ss "org.freedesktop.Secret.Service" "OpenSession" "plain" '((:string) "")))))
         (funcall fn session bus #'ss)))))

(defmacro with-open-session ((session  &optional (bus (gensym)) (secret-service-fn (gensym))) &body body)
  "Run BODY with SESSION and BUS bound to open session path and bus object, respectively."
  `(funcall-with-session (lambda (,session ,bus ,secret-service-fn)
                           (declare (ignorable ,bus))
                           (flet ((,secret-service-fn (&rest args)
                                    (apply ,secret-service-fn args)))
                             (declare (ignorable (function ,secret-service-fn)))
                             ,@body))))

;;; Secret items

(defun find-secrets (&rest pars)
  "Find secret objects that satisfy attributes given in PARS.

Returns two values, paths of unlocked objects and paths of locked objects."
  (dbus-call-method secrets-path "org.freedesktop.Secret.Service" "SearchItems" pars))

(defun find-all-secrets (&rest pars)
  "Find all secrets with attributes in PARS.

E.g., (find-all-secrets '(\"machine\" \"example.com\"))"
  (with-open-session (session bus ss)
    (ss "org.freedesktop.Secret.Service" "GetSecrets"
        (ss "org.freedesktop.Secret.Service" "SearchItems" pars) session)))

(defun stringify-secret (secret)
  "Turn data returned by D-Bus to a secret string"
  (map 'string 'code-char (third (second secret))))

(defun get-item-class-attributes (class item)
  "Get all attributes of ITEM of class CLASS."
  (mapcar (lambda (a) (cons (intern (string-upcase (car a)) "KEYWORD") (cdr a)))
          (dbus-call-method item "org.freedesktop.DBus.Properties" "GetAll" class)))


;(defstruct (secret :type list) session parameters value content-type)

(defun get-secret-item-attributes (item)
  "An alist of all item attributes. The cars of each item is a keyword."
  (get-item-class-attributes "org.freedesktop.Secret.Item" item))

(defun (setf get-secret-item-property) (value item label)
  (dbus-call-method item "org.freedesktop.DBus.Properties" "Set" "org.freedesktop.Secret.Item" label
                    (etypecase value
                      (string `((:string) ,value))
                      (cons `("a{ss}" ,value)))))

(defun get-secret-item-attribute (item label)
  (second (assoc label
                 (second (assoc :attributes (get-secret-item-attributes item))) :test 'equal)))

(defun create-item (collection-path label dict secret
                    &key replace (content-type "text/plain"))
  "Create an item.

  Collection-path is a path to the collection (e.g.,\"/org/freedesktop/secrets/collection/login\", LABEL name, DICT alist of attributes (all atoms strings), and SECRET the secret to store."
  (with-open-session (session bus)
    (with-introspected-object (ss2 bus collection-path secrets-service)
      (ss2 "org.freedesktop.Secret.Collection" "CreateItem"
           `(("org.freedesktop.Secret.Item.Label" ("s" ,label))
             ("org.freedesktop.Secret.Item.Attributes" ("a{ss}" ,dict)))
           (list session nil
                 (map '(vector (unsigned-byte 8)) 'char-code secret) ;; not correct
                 content-type)
           replace))))

(defun delete-secret (path)
  (with-open-bus (bus (session-server-addresses))
      (with-introspected-object (ss2 bus path secrets-service)
        (ss2 "org.freedesktop.Secret.Item" "Delete"))))


;;;; Collections
(defun get-collection-attributes (collection-path)
  (get-item-class-attributes "org.freedesktop.Secret.Collection" collection-path))

(defun get-collections-list ()
  (dbus-call-method secrets-path "org.freedesktop.DBus.Properties" "Get" "org.freedesktop.Secret.Service" "Collections"))

(defun make-object (path type)
  (with-open-bus (bus (session-server-addresses))
    (make-object-from-introspection (bus-connection bus) path type)))


;;;; ad-hoc
#+nil
(defun rename-secret (item)
  "Relabel all secrets labeled From-authinfo to machine/login. This was part of
migration from authinfo and not strictly needed."
  (let* ((attrs (get-secret-item-attributes item))
         (real-attrs (second (assoc "Attributes" attrs :test 'equal))))
    (when (equal (second (assoc "Label" attrs :test 'equal)) "From-authinfo")
      (set-secret-item-attribute item "Label" (format nil "~A/~A"
                                                      (second (assoc "machine" real-attrs :test 'equal))
                                                      (second (assoc "login" real-attrs :test 'equal)))))))
