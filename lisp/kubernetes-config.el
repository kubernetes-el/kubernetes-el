;;; kubernetes-config.el --- Configuration popup and supporting routines. -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(require 'kubernetes-ast)
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
    (clear-state . kubernetes-state-clear)
    (restart-client . kubernetes-client-restart)
    (context . kubernetes-state-context))
  "Functions to inject for isolation and testing.")

(magit-define-popup kubernetes-config-popup
  "Popup console for configuration."
  :group 'kubernetes
  :actions
  '((?n "Set namespace" kubernetes-config-set-namespace)))

(defun kubernetes-config-set-namespace (ns props)
  "Set the namespace to NS and restart the client process.

PROPS is an alist of functions to inject."
  (interactive (list (read-string "Namespace: " nil 'read-namespace)
                     kubernetes-config-props))
  (let ((props (or props kubernetes-config-props)))
    (kubernetes-props-bind ([set-namespace get-namespace restart-client clear-state] props)
      (unless (equal ns (get-namespace))
        (clear-state)
        (set-namespace ns)
        (restart-client)))))

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
