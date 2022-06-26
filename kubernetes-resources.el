;;; kubernetes-resources.el --- Discovering and reasoning about Kubernetes resources -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'json)
(require 'request)

(require 'kubernetes-core)
(require 'kubernetes-process)

(defun kubernetes--get-all-groups (&optional api-group-list)
  "Retrieve all API groups from the API-GROUP-LIST.

API-GROUP-LIST should be an alist representation of a APIGroupList resource.

If API-GROUP-LIST is not provided, this function will attempt to
query the cluster via proxy.  See `kubernetes-proxy'."
  (-when-let* ((api-group-list (or
                                api-group-list
                                (progn
                                  ;; TODO: Refactor `get-proxy-process' to return the ported process record object
                                  ;; rather than the raw process itself
                                  (get-proxy-process kubernetes--global-process-ledger)
                                  (request-response-data
                                   (kubernetes--request-option
                                    (format "%s/apis" (base-url (oref kubernetes--global-process-ledger proxy)))
                                    :parser 'json-read)))))
               (groups (--map (alist-get 'name it)
                              (alist-get 'groups api-group-list))))
    ;; TODO: Define a with-proxy-process macro that can take care of spinup and teardown
    (kill-proxy-process kubernetes--global-process-ledger)
    groups))

(defun kubernetes--get-all-kinds ()
  "Retrieve all resource kinds present on the current cluster.

Does nothing and prints a non-fatal error message if there is not
already an active kubectl proxy.  See `kubernetes-proxy'."
  (when (not (and (proxy-active-p kubernetes--global-process-ledger)
                (proxy-ready-p kubernetes--global-process-ledger)))
      (kubernetes--error "Kubectl proxy not active; start with `kubernetes-proxy' first")))

(provide 'kubernetes-resources)

;;; kubernetes-resources.el ends here
