;;; kubernetes-config.el --- Configuration popup and supporting routines. -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(require 'kubernetes-ast)
(require 'kubernetes-kubectl)
(require 'kubernetes-props)
(require 'kubernetes-state)
(require 'magit-popup)

(defgroup kubernetes nil
  "Emacs porcelain for Kubernetes."
  :group 'tools
  :prefix "kubernetes-")

(defconst kubernetes-config-props
  `((set-namespace . kubernetes-state-set-namespace)
    (get-namespace . kubernetes-state-namespace)
    (get-context . kubernetes-state-context)
    (clear-state . kubernetes-state-clear)
    (lookup-kubectl-settings . kubernetes-kubectl-kubeconfig-lookup-settings)
    (populate-from-kubectl-settings . kubernetes-state-populate-from-kubectl-settings)
    (reset-resources . kubernetes-state-reset-resources)
    (restart-client . kubernetes-client-restart))
  "Functions to inject for isolation and testing.")

;; Popup

(magit-define-popup kubernetes-config-popup
  "Popup console for configuration."
  :group 'kubernetes
  :actions
  '((?c "Set context" kubernetes-config-set-context)
    (?n "Set namespace" kubernetes-config-set-namespace)))

(defun kubernetes-config-set-namespace (ns &optional props)
  "Set the namespace to NS and restart the client process. PROPS is an alist of functions to inject."
  (interactive (list (kubernetes-kubectl-read-namespace)))
  (let ((props (or props kubernetes-config-props)))
    (kubernetes-props-bind ([set-namespace get-namespace restart-client reset-resources] props)
      (unless (equal ns (get-namespace))
        (reset-resources)
        (set-namespace ns)
        (restart-client)))))

(defun kubernetes-config-set-context (ctx &optional props)
  "Set the context to CTX and restart the client process. PROPS is an alist of functions to inject."
  (interactive (list (kubernetes-kubectl-read-context)))
  (let ((props (or props kubernetes-config-props)))
    (kubernetes-props-bind ([restart-client
                             get-context
                             clear-state
                             lookup-kubectl-settings populate-from-kubectl-settings] props)
      (unless (equal ctx (get-context))
        (clear-state)
        (populate-from-kubectl-settings (lookup-kubectl-settings ctx))
        (restart-client)))))


;; Components

(kubernetes-ast-define-component namespace (value)
  `(propertize (face kubernetes-namespace) ,value))

(kubernetes-ast-define-component context (value)
  `(propertize (face kubernetes-context) ,value))

(kubernetes-ast-define-component config (state)
  (let ((namespace (kubernetes-state-namespace state))
        (context (kubernetes-state-context state)))
    `(section (config)
              ,(when context
                 `(section (context)
                           (key-value 12 "Context" (context ,context))))
              ,(when namespace
                 `(section (namespace)
                           (key-value 12 "Namespace" (namespace ,namespace))))
              (padding))))

(provide 'kubernetes-config)

;;; kubernetes-config.el ends here
