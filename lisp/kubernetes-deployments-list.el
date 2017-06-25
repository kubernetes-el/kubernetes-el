;;; kubernetes-deployments-list.el --- Displays deployments  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)

(require 'kubernetes-ast)
(require 'kubernetes-state)
(require 'kubernetes-utils)
(require 'kubernetes-yaml)


;; Interactive commands

(defun kubernetes-deployments-list-display-deployment (deployment-name)
  "Show the deployment with string DEPLOYMENT-NAME at point in a pop-up buffer."
  (interactive (list (get-text-property (point) 'kubernetes-deployment-name)))
  (unless deployment-name
    (user-error "No deployment name at point"))

  (-if-let ((&hash (intern deployment-name) deployment) (kubernetes-state-deployments))
      (when-let (win (display-buffer (kubernetes-yaml-make-buffer (format "*kubernetes-deployment:%s*" deployment-name) deployment)))
        (select-window win))
    (user-error "Deployment %s not found and may have been deleted" deployment-name)))


;; Components

(kubernetes-ast-define-component deployment (deployment)
  (-let* (((&alist 'metadata (&alist 'name name
                                     'namespace namespace
                                     'creationTimestamp created-time)
                   'spec (&alist 'replicas desired-replicas
                                 'selector (&alist 'matchLabels (&alist "name" label)))
                   'status (&alist 'replicas actual-replicas))
           deployment)
          (section-name (intern (format "deployment-entry-%s" name))))

    `(section (,section-name t)
              (heading (deployment-name ,name))
              (indent
               (label-name ,label)
               (section (namespace) (key-value 12 "Namespace" (namespace ,namespace)))
               (key-value 12 "Replicas" ,(format "%s/%s" actual-replicas desired-replicas))
               (key-value 12 "Created" (relative-time ,created-time))
               (padding))
              (padding))))

(kubernetes-ast-define-component deployments-list (state &key deployments updated-p)
  (let ((updated-p (or updated-p (kubernetes-state-data-received-p state)))
        (deployments (or deployments (kubernetes-state-deployments state))))
    `(section (deployments-list nil)
              (heading "Deployments")
              (indent
               ,(if updated-p
                    (or (--map `(deployment ,(gethash it deployments)) (kubernetes-sorted-keys deployments))
                        `(empty-list-indicator))
                  `(loading-indicator)))
              (padding))))

(provide 'kubernetes-deployments-list)

;;; kubernetes-deployments-list.el ends here
