;;; kubernetes-resources.el --- Discovering and reasoning about Kubernetes resources -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'json)
(require 'request)

(require 'kubernetes-core)
(require 'kubernetes-process)

(defun kubernetes--get-all-groups (&optional api-group-list)
  "Retrieve all API group names from the API-GROUP-LIST.

API-GROUP-LIST should be an alist representation of a APIGroupList resource.

If API-GROUP-LIST is not provided, this function will attempt to
query the cluster via proxy.  It will error if a proxy server is
not already active.  See `kubernetes-proxy'."
  (-when-let* ((api-group-list (or
                                api-group-list
                                (kubernetes--require-proxy
                                 (request-response-data
                                  (kubernetes--request-option
                                   (format "%s/apis" (base-url (oref kubernetes--global-process-ledger proxy)))
                                   :parser 'json-read)))))
               (groups (--map (alist-get 'name it)
                              (alist-get 'groups api-group-list))))
    groups))

(defun kubernetes--preferred-version-for (group-name)
  "Query for the preferred version of the GROUP-NAME.

This function will error if a proxy server is not already active.
See `kubernetes-proxy'."
  (kubernetes--require-proxy
   (-let* ((url (format "%s/apis/%s"
                        (base-url (oref kubernetes--global-process-ledger proxy))
                        group-name))
           ((&alist 'preferredVersion (&alist 'version version))
            (request-response-data (kubernetes--request-option url :parser 'json-read))))
     version)))

(provide 'kubernetes-resources)

;;; kubernetes-resources.el ends here
