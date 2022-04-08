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

(defun find-secrets (&rest pars)
  "Find secret objects that satisfy attributes given in PARS.

Returns two values, paths of unlocked objects and paths of locked objects."
  (dbus-call-method secrets-path "org.freedesktop.Secret.Service" "SearchItems" pars))

(defun find-all-secrets (&rest pars)
  "Find all secrets with attributes in PARS.

E.g., (find-all-secrets '(\"machine\" \"example.com\"))"
  (with-open-bus (bus (session-server-addresses))
    (with-introspected-object (ss bus secrets-path secrets-service)
      (ss "org.freedesktop.Secret.Service" "GetSecrets"
             (ss "org.freedesktop.Secret.Service" "SearchItems" pars)
             (nth-value 1 (ss "org.freedesktop.Secret.Service" "OpenSession" "plain" '((:string) "")))))))

(defun stringify-secret (secret)
  "Turn data returned by D-Bus to a secret string"
  (map 'string 'code-char (third (second secret))))

(defun get-secret-item-attributes (item)
  (dbus-call-method item "org.freedesktop.DBus.Properties" "GetAll"  "org.freedesktop.Secret.Item"))

(defun set-secret-item-attribute (item label value)
  (dbus-call-method item "org.freedesktop.DBus.Properties" "Set" "org.freedesktop.Secret.Item" label `((:string) ,value)))

(defun make-object (path type)
  (with-open-bus (bus (session-server-addresses))
    (make-object-from-introspection (bus-connection bus) path type)))

(defun open-session ()
  (with-open-bus (bus (session-server-addresses))
    (with-introspected-object (ss bus secrets-path secrets-service)
      (ss "org.freedesktop.Secret.Service" "OpenSession" "plain" '((:string) "")))))

#+nil(defun rename-secret (item)
  "Relabel all secrets labeled From-authinfo to machine/login. This was part of
migration from authinfo and not strictly needed."
  (let* ((attrs (get-secret-item-attributes item))
         (real-attrs (second (assoc "Attributes" attrs :test 'equal))))
    (when (equal (second (assoc "Label" attrs :test 'equal)) "From-authinfo")
      (set-secret-item-attribute item "Label" (format nil "~A/~A"
                                                      (second (assoc "machine" real-attrs :test 'equal))
                                                      (second (assoc "login" real-attrs :test 'equal)))))))
