;;; kubernetes-popups.el --- Magit popups for Kubernetes buffers.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'magit-popup)
(require 'transient)
(require 'kubernetes-contexts)
(require 'kubernetes-process)
(require 'kubernetes-state)
(require 'kubernetes-utils)
(require 'kubernetes-el-tramp)

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

(transient-define-prefix kubernetes-proxy ()
  [["Connection"
    ("=p" "Port" "--port=" read-string)]]
  [["Actions"
    ;; TODO: Update this label dynamically based on current proxy status
    ("P" "Enable/disable" kubernetes-proxy-toggle)]])

(defun kubernetes-proxy-toggle (enable-disable args)
  "Enable/disable kubectl proxy according to ENABLE-DISABLE, using ARGS.

If disabling the proxy, ARGS is ignored."
  (interactive (list (not (proxy-active-p kubernetes--global-process-ledger))
                     (transient-args 'kubernetes-proxy)))
  (if enable-disable
      (get-proxy-process kubernetes--global-process-ledger args)
    (kill-proxy-process kubernetes--global-process-ledger)))

(transient-define-prefix kubernetes-dispatch ()
  [["Environment"
    ("c" "Configuration" kubernetes-config-popup)]
   ["Marks"
    ("D" "Delete" kubernetes-mark-for-delete)
    ("u" "Unmark" kubernetes-unmark)
    ("U" "Unmark (all)" kubernetes-unmark-all)]
   ["Commands"
    ("d" "Describe" kubernetes-describe)
    ("E" "Edit" kubernetes-edit)
    ("e" "Exec" kubernetes-exec)
    ("f" "File" kubernetes-file)
    ("L" "Labels" kubernetes-labels)
    ("l" "Logs" kubernetes-logs)
    ("P" "Proxy" kubernetes-proxy)]])

(transient-define-prefix kubernetes-exec ()
  "Execute into Kubernetes resources."
  :value '("--stdin" "--tty")
  ["Switches"
   ("-i" "Pass stdin to container" "--stdin")
   ("-t" "Stdin is a TTY" "--tty")]
  ["Options"
   ("=c" "Container to exec within" "--container=" :reader kubernetes-utils-read-container-name)]
  [["Actions"
    ("e" "Exec" kubernetes-exec-into)
    ("v" "Exec into container using vterm" kubernetes-exec-using-vterm
     :inapt-if-not (lambda () (require 'vterm nil 'noerror)))]])

(transient-define-prefix kubernetes-file ()
  "Work with files in Kubernetes resources."
  [["Options"
    ;; TODO: This doesn't currently get picked up by the suffixes
    ("=c" "Container" "--container=" kubernetes-utils-read-container-name)]]
  [["Actions"
    ("f" "Find file" kubernetes-tramp-find-file)
    ("d" "Dired" kubernetes-tramp-dired)]])

(transient-define-prefix kubernetes-describe ()
  "Describe Kubernetes resources."
  [["Actions"
    ("d" "Dwim" kubernetes-describe-dwim)
    ("p" "Pod" kubernetes-describe-pod)]])

(transient-define-prefix kubernetes-labels ()
  "Act on Kubernetes labels."
  [["Actions"
    ("p" "Pods" kubernetes-show-pods-for-label)]])

(transient-define-prefix kubernetes-edit ()
  "Edit Kubernetes resources."
  [["Actions"
    ("e" "Dwim" kubernetes-edit-resource-dwim)]])

(transient-define-prefix kubernetes-context ()
  "Work with kubectl contexts."
  [["Actions"
    ("r" "Rename a context" kubernetes-contexts-rename)
    ;; TODO: This suffix descriptor could be a little more colorful,
    ;; e.g. "Change from context <current-context-name> to...", but we can
    ;; improve later
    ("c" "Change current context" kubernetes-contexts-use-context)]])

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
        '((?c "Change context" kubernetes-contexts-use-context)
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
