(in-package #:secret-service)

(defsection @index
    (:title "Secret service CL API")
  "This is a partial interface to the [Secret Service API](https://specifications.freedesktop.org/secret-service/latest/index.html) in Common Lisp. Secret
service is an API to store keys and passwords in a dedicated service; both Gnome
and KDE provide such service (gnome-keyring).

It allows one to offload caring about how to safely store passwords and keys for
CL projects to someone else.

Simple usage:

```
  (find-the-secret '((\"machine\" \"example.com\")))
```
would return password for the secret with parameter machine having provided value,
 if there is only one, prompting for password if necessary.

```
(create-item (default-collection) \"My shop\" nil \"password\")
=> \"/org/freedesktop/secrets/collection/login/148\"
=> \"/\"
```

would create an item with just a label and a secret and return the path to it;
note that there is no property there that can be used to find the item by
search, but it shows (e.g., in Seahorse or emacs) properly.

```
(get-secret-of-item *)
=> \"password\"
```
would again reveal the password.
"
  (@find mgl-pax:section)
  (@manage mgl-pax:section)
  (@collections mgl-pax:section))

(defsection @find
    (:title "Find secrets")
  (find-all-secrets function)
  (get-secret-of-item function)
  (stringify-secret function)
  (get-secret-item-attributes function)
  (get-secret-item-attribute function)
  (get-secret-item-property function)
  (secret-item-search-error condition)
  (find-the-secret function))

(defsection @manage
    (:title "Manage secrets")
  (create-item function)
  (delete-secret function))

(define-glossary-term collection ()
                      "[Collection](https://specifications.freedesktop.org/secret-service/latest/ch03.html) is a group of secret service items (keyring, wallet). Each secret is part of a collection.

They are identified by the path. There are two commonly used collections - session and login.

Collection can be accessed by an alias. Alias DEFAULT should be always present.")

(defsection @collections
    (:title "Collections")
  (collection glossary-term)
  (default-collection function)
  (*login-collection* variable)
  (*session-collection* variable)
  (get-collections-list function)
  (find-collection-by-name function)
  (get-collection-by-alias function)
  (get-collection-attributes function))

;;;; (mgl-pax:update-asdf-system-html-docs secret-service::@index :secret-service)

;;;; D-Bus primer:
;;;; - path and secrets-service name give you an object.
;;;; - Object, its "type"/interface name (object has several and can be probed for)  and method name (again, part of objects hash tables) give you a function to call. And the function  needs parameters.
;;;; - Objects have also attributes/properties, and a separate interface to work with them.

(defvar secrets-service "org.freedesktop.secrets"
  "The D-Bus name used to talk to Secret Service.")

(defvar secrets-path "/org/freedesktop/secrets"
  "The D-Bus root object path used to talk to Secret Service.")

(defvar secrets-interface-item "org.freedesktop.Secret.Item"
  "A collection of items containing secrets.")

(defun invoke-secret-service-method (maybe-bus path method signature &rest args)
  (flet ((doit (bus)
             (invoke-method (bus-connection bus)
              method
              :destination secrets-service
              :path path
              :interface "org.freedesktop.Secret.Service"
              :signature signature
              :arguments args)))
    (if maybe-bus
        (doit maybe-bus)
        (with-open-bus (bus (session-server-addresses))
          (doit bus)))))

(defun invoke-properties-method (path method signature &rest args)
  (with-open-bus (bus (session-server-addresses))
    (invoke-method (bus-connection bus)
                   method
                   :destination secrets-service
                   :path path
                   :interface "org.freedesktop.DBus.Properties"
                   :signature signature
                   :arguments args)))


(defun get-session (bus)
  (nth-value 1 (invoke-secret-service-method bus secrets-path "OpenSession"
                                             "sv" "plain" '((:string) ""))))

;;; Secret items
(defstruct (secret (:type list))
  "A (possibly encoded) secret, see https://specifications.freedesktop.org/secret-service/latest/ch14.html"
  session       ; The session that was used to encode the secret
  parameters    ; Algorithm dependent parameters for secret value encoding
  value         ; Possibly encoded secret value
  content-type) ; The content type of the secret. For example: 'text/plain; charset=utf8'

(defun find-secret-paths (pars &optional bus)
  "Find secret objects that satisfy attributes given in PARS.

Returns two values, paths of unlocked objects and paths of locked objects."
  (invoke-secret-service-method bus secrets-path "SearchItems" "a{ss}" pars))

(defun find-all-secrets (pars)
  "Find all secrets with attributes in PARS.  Returns a list of
match pairs (path secret). See secret structure for the second item format.

E.g.,
```
(find-all-secrets '((\"machine\" \"example.com\")))
```
"
  (with-open-bus (bus (session-server-addresses))
    (invoke-secret-service-method bus secrets-path  "GetSecrets" "aoo"
                                  (find-secret-paths pars bus)
                                  (get-session bus))))

(define-condition secret-item-search-error (error)
  ((parameters :accessor get-parameters :initarg :parameters)
   (error-text :accessor get-error-text :initarg :text)
   (candidates :accessor get-candidates :initarg :candidates
               :initform nil))
  (:report (lambda (cond stream)
             (format stream "Cannot find single secret with ~{~{~a=~a~}~^, ~}: ~a ~@[~a~]"
                     (get-parameters cond)
                     (get-error-text cond)
                     (mapcar #'get-secret-item-label (get-candidates cond))))))

(define-condition secret-item-unlock-dismissed (error)
  ((path :accessor get-path :initarg :path))
  (:report (lambda (cond stream)
             (format stream "Password prompt was dismissed when opening secret ~a"
                     (get-secret-item-label (get-path cond))))))

(defun unlock-secret-item (path)
  "Unlock a secret item on PATH."
  (with-open-bus (bus (session-server-addresses))
          (multiple-value-bind (done prompt)
              (unlock-paths (list path) bus)
            (unless done
              (with-introspected-object (p bus prompt secrets-service)
                (p "org.freedesktop.Secret.Prompt" "Prompt" ""))
              (loop
                (sleep 0.1) ; can we wait instead of polling?
                (let ((message (receive-message-no-hang (bus-connection bus))))
                  (cond ((and (typep message 'signal-message)
                              (equal (message-member message) "Completed"))
                         (if (car (message-body message)) ; dismissed?
                             (error 'secret-item-unlock-dismissed :path path)
                             (return))))))))))

(defun get-secret-of-item (path)
  "Get secret from secret item on PATH. Second value is path itself.

Provide restart that tries to unlock and read again. This should not be standard
situation, but, as API standard says, The inherent race conditions present due
to this are unavoidable, and must be handled gracefully."
  (restart-case
      (values
       (stringify-secret
        (with-open-bus (bus (session-server-addresses))
          (with-introspected-object (item bus path secrets-service)
            (item secrets-interface-item "GetSecret" (get-session bus)))))
       path)
    (attempt-unlock (err)
      ;; can we do better? Is this guaranteed to work on other backends?
      (when (equalp (method-error-arguments err)
                    '("Cannot get secret of a locked object"))
        (unlock-secret-item path)
        (get-secret-of-item path)))))

(defun find-the-secret (pars)
  "Make sure that there is just one secret matching pars, and return it.
Raise error otherwise, or when the secret needs to be unlocked."
  (multiple-value-bind (unlocked locked)
      (find-secret-paths pars)
    (cond
      ((and unlocked (null locked) (null (cdr unlocked)))
       (get-secret-of-item (first unlocked)))
      ((and locked (null unlocked) (null (cdr locked)))
       (unlock-secret-item (car locked))
       (get-secret-of-item (car locked)))
      ((and (null locked) (null unlocked)
            (error 'secret-item-search-error :parameters pars
                                             :text "No matching secret")))
      (t
       (restart-case
           (error 'secret-item-search-error :parameters pars
                                            :text "More that one matching item"
                                            :candidates (append unlocked locked)))))))

(defun stringify-secret (secret)
  "Turn secret structure returned by D-Bus to a secret string"
  (if (stringp secret) secret
    (let ((val (secret-value secret)))
      (etypecase val
        ((string) val)
        ((or cons vector) (map 'string 'code-char val))))))

;;;; Secret items have fixed list ofproperties, and one of them is an alist attributes.
;;;; Searching is done on attributes.

(defun get-item-class-attributes (class item)
  "Get all attributes of ITEM of class CLASS."
  (mapcar (lambda (a) (cons (intern (string-upcase (car a)) "KEYWORD") (cdr a)))
          (invoke-properties-method item "GetAll" "s" class)))

(defun get-secret-item-properties (item)
  "An alist of all item attributes. The cars of each item is a keyword."
  (get-item-class-attributes secrets-interface-item item))

(defun get-secret-item-property (item-path label)
  "Get attribute LABEL of secret item with path ITEM-PATH."
  (second (assoc label (get-secret-item-properties item-path) :test #'equal)))

(defun (setf get-secret-item-property) (value item label)
  (invoke-properties-method item "Set" "ssv" secrets-interface-item label
                            (etypecase value
                              (string `((:string) ,value))
                              (cons `("a{ss}" ,value)))))

(defun get-secret-item-label (item-path)
  "Get label of item with ITEM-PATH"
  (get-secret-item-property item-path :label ))

(defun get-secret-item-attributes (item-path)
  "An alist of all item attributes. The cars of each item is a keyword."
  (get-secret-item-property item-path :attributes))

(defun get-secret-item-attribute (item label)
  "Get attribute of a secret item"
  (second (assoc label (get-secret-item-attributes item) :test 'equal)))

(defun create-item (collection-path label dict secret
                    &key replace (content-type "text/plain"))
  "Create an item.

  Collection-path is a path to the COLLECTION that should store the secret LABEL is name of the secret, DICT alist of attributes (all atoms strings), and SECRET the secret to store."
  (with-open-bus (bus (session-server-addresses))
    (with-introspected-object (ss2 bus collection-path secrets-service)
      (ss2 "org.freedesktop.Secret.Collection" "CreateItem"
           `(("org.freedesktop.Secret.Item.Label" ("s" ,label))
             ("org.freedesktop.Secret.Item.Attributes" ("a{ss}" ,dict)))
           (make-secret :session (get-session bus)
                        :value (map '(vector (unsigned-byte 8)) 'char-code secret) ;; not correct
                        :content-type content-type)
           replace))))

(defun delete-secret (path)
  (with-open-bus (bus (session-server-addresses))
      (with-introspected-object (ss2 bus path secrets-service)
        (ss2 secrets-interface-item "Delete"))))


;;;; Collections
(defvar *session-collection* "/org/freedesktop/secrets/collection/session"
  "Path to the collection named session. Gnome keyring provides this collection; it has lifetime of the logged in user session.

It may or may not exist with other Service Providers.")

(defvar *login-collection* "/org/freedesktop/secrets/collection/login"
  "Path to the collection named login. This is a commonly used collection.")

(defun get-collection-attributes (collection-path)
  (get-item-class-attributes "org.freedesktop.Secret.Collection" collection-path))

(defun get-collections-list ()
  "List of available collections."
  (invoke-properties-method secrets-path "Get" "ss" "org.freedesktop.Secret.Service" "Collections"))

(defun nil-if-slash (object)
  "Several functions return \"/\" as not found. Translate this to nil."
  (unless (equal object "/") object))

(defun get-collection-by-alias (name)
  "Get collection path by alias name, or nil. There is one predefined alias, \"session\". "
  (nil-if-slash
   (invoke-secret-service-method nil secrets-path "ReadAlias" "s" name)))

(defun find-collection-by-name (name)
  "Find path to collection with label or alias NAME."
  (or (get-collection-by-alias name)
      (find name (get-collections-list)
            :key (lambda (p) (second (assoc :label (get-collection-attributes p))))
            :test 'equal)))

(defun default-collection ()
  "Default collection, that is, collection with alias default."
  (get-collection-by-alias "default"))


;;;; Locking
(defun lock-paths (objects)
  (invoke-secret-service-method nil secrets-path "Lock" "ao" objects))

(defun unlock-paths (objects &optional bus)
  (invoke-secret-service-method bus secrets-path "Unlock" "ao" objects))
