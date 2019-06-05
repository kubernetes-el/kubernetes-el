;;; kubernetes-popups.el --- Magit popups for Kubernetes buffers.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'magit-popup)
(require 'kubernetes-state)

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

(magit-define-popup kubernetes-logs-popup
  "Popup console for pod logging commands."
  :group 'kubernetes
  :switches
  '((?p "Print logs for previous instances of the container in this pod" "-p"))
  :options
  '("Options for customizing logging behaviour"
    (?t "Number of lines to display" "--tail=" read-number)
    (?c "Container to read logs from" "--container=" kubernetes-utils-read-container-name)
    "Time controls"
    (?s "Since relative time" "--since=" kubernetes-utils-read-time-value)
    (?d "Since absolute datetime" "--since-time=" kubernetes-utils-read-iso-datetime))
  :actions
  '((?l "Logs" kubernetes-logs-fetch-all)
    (?f "Logs (stream and follow)" kubernetes-logs-follow))
  :max-action-columns 2
  :default-action 'kubernetes-logs)

(magit-define-popup kubernetes-overview-popup
  "Popup console for showing an overview of available popup commands."
  :group 'kubernetes
  :actions
  '("Environment"
    (?c "Configuration" kubernetes-config-popup)
    "Marks"
    (?D "Delete" kubernetes-mark-for-delete)
    (?u "Unmark" kubernetes-unmark)
    (?U "Unmark all" kubernetes-unmark-all)
    "Popup commands"
    (?d "Describe" kubernetes-describe-popup)
    (?e "Exec" kubernetes-exec-popup)
    (?L "Labels" kubernetes-labels-popup)
    (?l "Logs" kubernetes-logs-popup)
    "Misc"
    (?h "Describe mode and keybindings" describe-mode)))

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
  '((?e "Exec" kubernetes-exec-into))
  :default-action 'kubernetes-exec-into)

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
