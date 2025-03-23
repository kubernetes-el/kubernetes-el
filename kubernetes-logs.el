;;; kubernetes-logs.el --- Utilities for working with log buffers.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'subr-x)

(require 'kubernetes-modes)
(require 'kubernetes-pods)
(require 'kubernetes-utils)

(autoload 'json-pretty-print-buffer "json")

(require 'kubernetes-vars)

(defconst kubernetes-logs-supported-resource-types
  '("pod" "deployment" "statefulset" "job" "service")
  "List of Kubernetes resource types that support the `kubectl logs` command.")

;; New variable to define buffer name format
(defvar kubernetes-logs-buffer-name-format "*kubernetes logs: %s%s/%s%s*"
  "Format for Kubernetes logs buffer names.
First %s is replaced with the namespace prefix if specified (including trailing slash),
second %s is the resource type,
third %s is the resource name,
fourth %s is replaced with container suffix if specified (including the leading colon).")

(defun kubernetes-logs--generate-buffer-name (resource-type resource-name args state)
  "Generate a buffer name for logs of RESOURCE-TYPE named RESOURCE-NAME with ARGS in STATE.
Extracts container name from ARGS if present and includes namespace information."
  (let* ((container-name (kubernetes-utils--extract-container-name-from-args args))
         (container-suffix (if container-name (format ":%s" container-name) ""))
         (namespace (kubernetes-state--get state 'current-namespace))
         (namespace-prefix (if namespace (format "%s/" namespace) "")))
    (format kubernetes-logs-buffer-name-format namespace-prefix resource-type resource-name container-suffix)))

(defun kubernetes-logs--read-resource-if-needed (state)
  "Read a resource from the minibuffer if none is at point using STATE.
Returns a cons cell of (type . name)."
  (or (when-let ((resource-info (kubernetes-utils-get-resource-info-at-point)))
        (when (member (car resource-info) kubernetes-logs-supported-resource-types)
          resource-info))
      ;; No loggable resource at point, default to pod selection
      (cons "pod" (kubernetes-pods--read-name state))))

(defun kubernetes-logs--log-line-buffer-for-string (s)
  "Create a buffer to display log line S."
  (let ((propertized (with-temp-buffer
                       (insert s)
                       (goto-char (point-min))
                       (when (equal (char-after) ?\{)
                         (json-pretty-print-buffer)
                         (funcall kubernetes-json-mode)
                         (font-lock-ensure))
                       (buffer-string))))

    (with-current-buffer (get-buffer-create kubernetes-log-line-buffer-name)
      (kubernetes-log-line-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert propertized)
        (goto-char (point-min)))
      (current-buffer))))

;;;###autoload
(defun kubernetes-logs-inspect-line (pos)
  "Show detail for the log line at POS."
  (interactive "d")
  (display-buffer (kubernetes-logs--log-line-buffer-for-string
                   (save-excursion
                     (goto-char pos)
                     (buffer-substring (line-beginning-position) (line-end-position))))))

;;;###autoload
(defun kubernetes-logs-previous-line ()
  "Move backward and inspect the line at point."
  (interactive)
  (forward-line -1)
  (when (get-buffer kubernetes-log-line-buffer-name)
    (kubernetes-logs-inspect-line (point))))

;;;###autoload
(defun kubernetes-logs-forward-line ()
  "Move forward and inspect the line at point."
  (interactive)
  (forward-line 1)
  (when (get-buffer kubernetes-log-line-buffer-name)
    (kubernetes-logs-inspect-line (point))))

;;;###autoload
(defun kubernetes-logs-follow (args state)
  "Open a streaming logs buffer for a resource at point or selected by user.
ARGS are additional args to pass to kubectl.
STATE is the current application state."
  (interactive
   (let ((state (kubernetes-state)))
     (list (transient-args 'kubernetes-logs)
           state)))
  (let* ((resource-info (kubernetes-logs--read-resource-if-needed state))
         (resource-type (car resource-info))
         (resource-name (cdr resource-info)))
    (kubernetes-logs-fetch-all resource-type resource-name (cons "-f" args) state)))

;;;###autoload
(defun kubernetes-logs-fetch-all (resource-type resource-name args state)
  "Open a streaming logs buffer for a resource.

RESOURCE-TYPE is the type of resource (pod, deployment, statefulset, job, service).
RESOURCE-NAME is the name of the resource to log.
ARGS are additional args to pass to kubectl.
STATE is the current application state."
  (interactive
   (let* ((state (kubernetes-state))
          (resource-info (kubernetes-logs--read-resource-if-needed state)))
     (list (car resource-info)
           (cdr resource-info)
           (transient-args 'kubernetes-logs)
           state)))

  ;; Format the resource in the kubectl resource/name format consistently for all resource types
  (let* ((resource-path (format "%s/%s" resource-type resource-name))
         ;; Build clean args with namespace at the end
         (namespace-arg (when-let (ns (kubernetes-state--get state 'current-namespace))
                          (list (format "--namespace=%s" ns))))
         (kubectl-args (append (list "logs")
                              args
                              (list resource-path)
                              (kubernetes-kubectl--flags-from-state state)
                              namespace-arg))
         (buffer-name (kubernetes-logs--generate-buffer-name resource-type resource-name args state)))
    (with-current-buffer (kubernetes-utils-process-buffer-start buffer-name #'kubernetes-logs-mode kubernetes-kubectl-executable kubectl-args)
      ;; Set buffer-local variables to store information about this log buffer
      (setq-local kubernetes-logs-resource-type resource-type)
      (setq-local kubernetes-logs-resource-name resource-name)
      (setq-local kubernetes-logs-namespace (kubernetes-state--get state 'current-namespace))
      (setq-local kubernetes-logs-container-name (kubernetes-utils--extract-container-name-from-args args))
      ;; Store the complete kubectl command args for direct reuse during refresh
      (setq-local kubernetes-logs-kubectl-args kubectl-args)
      (select-window (display-buffer (current-buffer))))))

;; Function to refresh logs in the current buffer
;;;###autoload
(defun kubernetes-logs-refresh ()
  "Refresh logs in the current buffer."
  (interactive)
  (when (derived-mode-p 'kubernetes-logs-mode)
    (if (boundp 'kubernetes-logs-kubectl-args)
        ;; Simply reuse the exact kubectl args stored in the buffer
        (with-current-buffer (kubernetes-utils-process-buffer-start (buffer-name)
                                                                    #'kubernetes-logs-mode
                                                                    kubernetes-kubectl-executable
                                                                    kubernetes-logs-kubectl-args)
          ;; No need to set local variables again as they're already set
          (message "Logs refreshed"))
      (message "Cannot refresh logs: command information not available"))))

(transient-define-prefix kubernetes-logs ()
  "Fetch or tail logs from Kubernetes resources."
  [["Flags"
    ("-a" "Print logs from all containers in this pod" "--all-containers=true")
    ("-p" "Print logs for previous instances of the container in this pod" "-p")
    ("-t" "Include timestamps on each line in the log output" "--timestamps=true")
    ("-A" "Get logs from all pods" "--all-pods=true")
    ("-P" "Prefix each log line with pod name and container name" "--prefix=true")]
   ["Options"
    ("=c" "Select container" "--container=" kubernetes-utils-read-container-name)
    ("=t" "Number of lines to display" "--tail=" transient-read-number-N+)]
   ["Time"
    ("=s" "Since relative time" "--since=" kubernetes-utils-read-time-value)
    ("=d" "Since absolute datetime" "--since-time=" kubernetes-utils-read-iso-datetime)]]
  [["Actions"
    ("l" "Logs" kubernetes-logs-fetch-all)
    ("f" "Logs (stream and follow)" kubernetes-logs-follow)
    ("b" "Switch logs buffer" kubernetes-logs-switch-buffers)]])

;;;###autoload
(defvar kubernetes-logs-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "n") #'kubernetes-logs-forward-line)
    (define-key keymap (kbd "p") #'kubernetes-logs-previous-line)
    (define-key keymap (kbd "RET") #'kubernetes-logs-inspect-line)
    (define-key keymap (kbd "g") #'kubernetes-logs-refresh)
    (define-key keymap (kbd "M-w") nil)
    keymap)
  "Keymap for `kubernetes-logs-mode'.")

;;;###autoload
(define-derived-mode kubernetes-logs-mode kubernetes-mode "Kubernetes Logs"
  "Mode for displaying and inspecting Kubernetes logs.

\\<kubernetes-logs-mode-map>\
Type \\[kubernetes-logs-inspect-line] to open the line at point in a new buffer.
Type \\[kubernetes-logs-refresh] to refresh the logs in the current buffer.

\\{kubernetes-logs-mode-map}"
  ;; Override parent mode's keymap to ensure our bindings take precedence
  (use-local-map kubernetes-logs-mode-map))

;;;###autoload
(defvar kubernetes-log-line-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "n") #'kubernetes-logs-forward-line)
    (define-key keymap (kbd "p") #'kubernetes-logs-previous-line)
    keymap)
  "Keymap for `kubernetes-log-line-mode'.")

;;;###autoload
(define-derived-mode kubernetes-log-line-mode kubernetes-mode "Log Line"
  "Mode for inspecting Kubernetes log lines.

\\{kubernetes-log-line-mode-map}")

(defun kubernetes-logs--get-log-buffers ()
  "Get all Kubernetes log buffers in the current Emacs session.
Returns a list of buffer objects."
  (let ((log-buffers '()))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (derived-mode-p 'kubernetes-logs-mode)
          (push buf log-buffers))))
    log-buffers))

;;;###autoload
(defun kubernetes-logs-switch-buffers ()
  "List all Kubernetes log buffers and allow selecting one."
  (interactive)
  (let ((log-buffers (kubernetes-logs--get-log-buffers)))
    (if (null log-buffers)
        (message "No Kubernetes logs buffers found")
      ;; Use completing-read with buffer category annotation for embark
      ;; Use the actual buffer names directly to avoid truncation issues
      (let* ((buffer-names (mapcar #'buffer-name log-buffers))
             (selected-name (completing-read
                             "Select logs buffer: "
                             (lambda (string pred action)
                               (if (eq action 'metadata)
                                   '(metadata (category . buffer))
                                 (complete-with-action
                                  action buffer-names string pred))))))
        (when selected-name
          (switch-to-buffer (get-buffer selected-name)))))))

(provide 'kubernetes-logs)

;;; kubernetes-logs.el ends here
