;;; kubernetes-popups.el --- Magit popups for Kubernetes buffers.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'magit-popup)
(require 'transient)
(require 'kubernetes-state)
(require 'kubernetes-utils)

(defgroup kubernetes nil
  "Emacs porcelain for Kubernetes."
  :group 'tools
  :prefix "kubernetes-")


;; Utilities

(defun kubernetes-popups--read-existing-file (prompt &optional default)
  (read-file-name prompt nil default t nil #'file-regular-p))

(defun kubernetes-popups--read-server-and-port (&optional _option defaults)
  (-let* (((server port) (split-string (or defaults "") ":" t))
          (port (or (ignore-errors (string-to-number port)) 8080)))
    (format "%s:%s" (read-string "Server: " server) (read-number "Port: " port))))


;; Popup definitions

(transient-define-prefix kubernetes-dispatch ()
  [["Environment"
    ("c" "Configuration" kubernetes-config-popup)]
   ["Marks"
    ("D" "Delete" kubernetes-mark-for-delete)
    ("u" "Unmark" kubernetes-unmark)
    ("U" "Unmark (all)" kubernetes-unmark-all)]
   ["Commands"
    ("d" "Describe" kubernetes-describe-popup)
    ("E" "Edit" kubernetes-edit-popup)
    ("e" "Exec" kubernetes-exec-popup)
    ("f" "File" kubernetes-file-popup)
    ("L" "Labels" kubernetes-labels-popup)
    ("l" "Logs" kubernetes-logs)]])

(magit-define-popup kubernetes-exec-popup
  "Popup console for exec commands for POD."
  :group 'kubernetes
  :default-arguments '("-i" "-t")
  :switches
  '((?i "Pass stdin to container" "-i" t)
    (?t "Stdin is a TTY" "-t" t))
  :options
  '("Options for customizing exec behaviour"
    (?c "Container to exec within" "--container=" kubernetes-utils-read-container-name))
  :actions
  '((?e "Exec" kubernetes-exec-into)
    (?v "Exec into container using vterm" kubernetes-exec-using-vterm))
  :default-action 'kubernetes-exec-into)

(magit-define-popup kubernetes-file-popup
  "Popup console for file commands for POD."
  :group 'kubernetes
  :options
  '("Options for customizing file behaviour"
    (?c "Container" "--container=" kubernetes-utils-read-container-name))
  :actions
  '((?f "Find file" kubernetes-tramp-find-file)
    (?d "Dired" kubernetes-tramp-dired))
  :default-action 'kubernetes-tramp-dired)

(magit-define-popup kubernetes-describe-popup
  "Popup console for describe commands."
  :group 'kubernetes
  :actions
  '((?d "Dwim" kubernetes-describe-dwim)
    (?p "Pod" kubernetes-describe-pod))
  :default-action 'kubernetes-logs)

(magit-define-popup kubernetes-labels-popup
  "Popup console for commands relating to labels."
  :group 'kubernetes
  :actions
  '((?p "Pods" kubernetes-show-pods-for-label))
  :default-action 'kubernetes-show-pods-for-label)

(magit-define-popup kubernetes-edit-popup
  "Popup console for commands relating to edit resources."
  :group 'kubernetes
  :actions
  '((?e "Edit (dwim)" kubernetes-edit-resource-dwim))
  :default-action 'kubernetes-edit-resource-dwim)


;; Config popup
;;
;; The macro definition is expanded here and modified to support marshalling
;; state between the state manager and the magit popup's global state.

(defconst kubernetes-config-popup
  (list :group 'kubernetes
        :variable 'kubernetes-kubectl-flags
        :options
        `("Configuration"
          (?f "Kubeconfig file" "--kubeconfig=" kubernetes-popups--read-existing-file)
          (?l "Cluster name in config" "--cluster=" read-string)
          (?s "Server address and port" "--server=" kubernetes-popups--read-server-and-port)
          (?u "User in kubeconfig" "--user=" read-string)
          "Authentication"
          (?a "CA cert file" "--certificate-authority=" kubernetes-popups--read-existing-file)
          (?k "Client key file" "--client-key=" kubernetes-popups--read-existing-file)
          (?r "Client cert file" "--client-certificate=" kubernetes-popups--read-existing-file)
          (?t "API token" "--token=" read-string))
        :actions
        '((?c "Change context" kubernetes-use-context)
          (?n "Set namespace" kubernetes-set-namespace))))

(defun kubernetes-popups--update-kubectl-state ()
  ;; HACK: Need to use internal magit vars, since this is run inside the popup
  ;; refresh hook.
  (when (eq magit-this-popup 'kubernetes-config-popup)
    (kubernetes-state-update-kubectl-flags (magit-popup-get-args))))

(defun kubernetes-config-popup (&optional arg)
  "Popup console for showing an overview of available config commands.

With ARG, execute default command."
  (interactive "P")
  (let ((flags (kubernetes-state-kubectl-flags (kubernetes-state))))
    (setq kubernetes-kubectl-flags flags))
  (add-hook 'magit-refresh-popup-buffer-hook #'kubernetes-popups--update-kubectl-state)
  (magit-invoke-popup 'kubernetes-config-popup nil arg))

(magit-define-popup-keys-deferred 'kubernetes-config-popup)

(provide 'kubernetes-popups)

;;; kubernetes-popups.el ends here
