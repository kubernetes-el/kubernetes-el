;;; kubernetes-vars.el --- Customizable interface for Kubernetes package.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defgroup kubernetes nil
  "Emacs porcelain for Kubernetes."
  :group 'tools
  :prefix "kubernetes-")

(defcustom kubernetes-kubectl-executable "kubectl"
  "The kubectl command used for Kubernetes commands."
  :group 'kubernetes
  :type 'string)

(defcustom kubernetes-utils-display-buffer-select t
  "Whether to select Kubernetes buffers automatically."
  :group 'kubernetes
  :type 'boolean)

(defcustom kubernetes-utils-display-buffer-function 'kubernetes-utils-display-buffer-fullframe
  "The function used display a Kubernetes buffer.

The function must take a single argument, which is the buffer to display."
  :group 'kubernetes
  :type '(radio (function-item kubernetes-utils-display-buffer-fullframe)
                (function-item display-buffer)
                (function :tag "Function")))

(defcustom kubernetes-pod-restart-warning-threshold 5
  "The threshold for pod restarts above which a pod is highlighted."
  :group 'kubernetes
  :type 'number)

(defcustom kubernetes-poll-frequency 5
  "The frequency at which to poll Kubernetes for changes."
  :group 'kubernetes
  :type 'integer)

(defcustom kubernetes-redraw-frequency 5
  "The buffer redraw frequency in seconds.

This is the frequency at which Kubernetes buffers will be redrawn
to match the current state.  This variable should be tuned to
balance interface stuttering with update frequency."
  :group 'kubernetes
  :type 'integer)

(defcustom kubernetes-json-mode 'javascript-mode
  "The mode to use when rendering pretty-printed JSON."
  :group 'kubernetes
  :type 'function)

(defcustom kubernetes-default-exec-command "bash"
  "The default command to use when exec'ing into a pod's container."
  :group 'kubernetes
  :type 'string)

(defcustom kubernetes-clean-up-interactive-exec-buffers t
  "If non-nil, automatically kill interactive exec buffers on process exit."
  :group 'kubernetes
  :type 'boolean)

(defcustom kubernetes-minimum-error-display-time 10
  "Minimum time in seconds for which errors will be displayed in overview buffer."
  :group 'kubernetes
  :type 'integer)

(defface kubernetes-context-name
  '((((class color) (background light)) :foreground "SkyBlue4")
    (((class color) (background  dark)) :foreground "LightSkyBlue1"))
  "Face for context names in report buffers."
  :group 'kubernetes)

(defface kubernetes-json-key
  '((((class color) (background light)) :foreground "grey30" :weight bold)
    (((class color) (background  dark)) :foreground "grey80" :weight bold))
  "Face for keys in pretty-printed parsed JSON."
  :group 'kubernetes)

(defface kubernetes-progress-indicator
  '((t :inherit shadow))
  "Face for progress indicators."
  :group 'kubernetes)

(defface kubernetes-pending-deletion
  '((t :inherit shadow :strike-through t))
  "Face for pods awaiting deletion."
  :group 'kubernetes)

(defface kubernetes-delete-mark
  '((t :inherit error))
  "Face for deletion mark indicators."
  :group 'kubernetes)


(defconst kubernetes-display-pods-buffer-name "*kubernetes pods*")

(defconst kubernetes-display-config-buffer-name "*kubernetes config*")

(defconst kubernetes-display-configmap-buffer-name "*kubernetes configmap*")

(defconst kubernetes-display-service-buffer-name "*kubernetes service*")

(defconst kubernetes-display-configmaps-buffer-name "*kubernetes configmaps*")

(defconst kubernetes-display-secret-buffer-name "*kubernetes secret*")

(defconst kubernetes-display-secrets-buffer-name "*kubernetes secrets*")

(defconst kubernetes-overview-buffer-name "*kubernetes overview*")

(defconst kubernetes-log-line-buffer-name "*log line*")

(defconst kubernetes-logs-buffer-name "*kubernetes logs*")

(defconst kubernetes-pod-buffer-name "*kubernetes pod*")

(defconst kubernetes-exec-buffer-name "*kubernetes exec*")


(defvar kubernetes-poll-hook nil
  "Hook run every time polling should occur.")

(defvar kubernetes-redraw-hook nil
  "Hook run every time redrawing should occur.")


(provide 'kubernetes-vars)

;;; kubernetes-vars.el ends here
