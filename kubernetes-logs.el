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

(defun kubernetes-logs--read-resource-with-prompt (state)
  "Prompt user to select a resource for logs using STATE.
Returns a cons cell of (type . name)."
  (condition-case nil
      (let* ((resource-type (completing-read "Resource type: " kubernetes-logs-supported-resource-types nil t))
             (resource-name (kubernetes-utils-get-resource-name state resource-type)))
        (cons resource-type resource-name))
    ;; Clear selection on abort
    (quit
     (setq kubernetes-utils--selected-resource nil)
     (signal 'quit nil))))

(defun kubernetes-logs-select-resource ()
  "Select a resource for logs and store it for use in the transient."
  (interactive)
  (let* ((state (kubernetes-state))
         (resource-info nil))
    ;; Use condition-case to catch keyboard quit
    (condition-case nil
        (setq resource-info (kubernetes-logs--read-resource-with-prompt state))
      (quit (setq kubernetes-utils--selected-resource nil)
            (error "Selection canceled")))

    (when resource-info
      (setq kubernetes-utils--selected-resource resource-info)
      (message "Selected %s/%s for logs" (car resource-info) (cdr resource-info))
      (transient-setup 'kubernetes-logs))))

;;;###autoload
(cl-defun kubernetes-logs-fetch-all (resource-type resource-name args state)
  "Open a logs buffer for a resource.

RESOURCE-TYPE is the type of resource (pod, deployment, statefulset, job, service).
RESOURCE-NAME is the name of the resource to log.
ARGS are additional args to pass to kubectl.
STATE is the current application state."
  (interactive
   (let* ((state (kubernetes-state))
          (resource-info (kubernetes-utils-read-resource-if-needed state kubernetes-logs-supported-resource-types "pod")))
     (list (car resource-info)
           (cdr resource-info)
           (transient-args 'kubernetes-logs)
           state)))

  (let* ((resource-path (format "%s/%s" resource-type resource-name))
         ;; Build clean args with namespace at the end
         (namespace-arg (when-let (ns (kubernetes-state--get state 'current-namespace))
                          (list (format "--namespace=%s" ns))))
         (kubectl-args (append (list "logs")
                              args
                              (list resource-path)
                              (kubernetes-kubectl--flags-from-state state)
                              namespace-arg))
         (buffer-name (kubernetes-utils-generate-operation-buffer-name
                       "logs" resource-type resource-name args state)))
    (with-current-buffer (kubernetes-utils-process-buffer-start buffer-name #'kubernetes-logs-mode kubernetes-kubectl-executable kubectl-args)
      ;; Set buffer-local variables to store information about this log buffer
      (setq-local kubernetes-logs-resource-type resource-type)
      (setq-local kubernetes-logs-resource-name resource-name)
      (setq-local kubernetes-logs-namespace (kubernetes-state--get state 'current-namespace))
      (setq-local kubernetes-logs-container-name (kubernetes-utils--extract-container-name-from-args args))
      ;; Store the complete kubectl command args for direct reuse during refresh
      (setq-local kubernetes-logs-kubectl-args kubectl-args)
      (select-window (display-buffer (current-buffer))))))

;;;###autoload
(cl-defun kubernetes-logs-follow (resource-type resource-name args state)
  "Open a streaming logs buffer for a resource.

RESOURCE-TYPE is the type of resource (pod, deployment, statefulset, job, service).
RESOURCE-NAME is the name of the resource to log.
ARGS are additional args to pass to kubectl.
STATE is the current application state."
  (interactive
   (let* ((state (kubernetes-state))
          (resource-info (kubernetes-utils-read-resource-if-needed state kubernetes-logs-supported-resource-types "pod")))
     (list (car resource-info)
           (cdr resource-info)
           (transient-args 'kubernetes-logs)
           state)))
  (kubernetes-logs-fetch-all resource-type resource-name (cons "-f" args) state))

;; Define compatibility versions of the functions that take 2 args
;;;###autoload
(cl-defun kubernetes-logs-follow (args state)
  "Open a streaming logs buffer for a resource at point or selected by user.
ARGS are additional args to pass to kubectl.
STATE is the current application state."
  (let* ((resource-info (kubernetes-utils-read-resource-if-needed state kubernetes-logs-supported-resource-types "pod"))
         (resource-type (car resource-info))
         (resource-name (cdr resource-info)))
    (kubernetes-logs-fetch-all resource-type resource-name (cons "-f" args) state)))

;; Function to handle with-check behavior
(defun kubernetes-logs-fetch-all-with-check (args state)
  "Call `kubernetes-logs-fetch-all' if a resource is available.
ARGS and STATE are passed to `kubernetes-logs-fetch-all'."
  (interactive
   (list (transient-args 'kubernetes-logs)
         (kubernetes-state)))
  (unless (kubernetes-utils-has-valid-resource-p kubernetes-logs-supported-resource-types)
    (user-error "No resource selected. Press 'r' to select a resource"))

  (let* ((resource-info (kubernetes-utils-get-effective-resource state kubernetes-logs-supported-resource-types))
         (resource-type (car resource-info))
         (resource-name (cdr resource-info)))
    ;; Clear the manual selection after logs are fetched
    (setq kubernetes-utils--selected-resource nil)
    (kubernetes-logs-fetch-all resource-type resource-name args state)))

;; Function to handle with-check follow behavior
(defun kubernetes-logs-follow-with-check (args state)
  "Call `kubernetes-logs-follow' if a resource is available.
ARGS and STATE are passed to `kubernetes-logs-follow'."
  (interactive
   (list (transient-args 'kubernetes-logs)
         (kubernetes-state)))
  (unless (kubernetes-utils-has-valid-resource-p kubernetes-logs-supported-resource-types)
    (user-error "No resource selected. Press 'r' to select a resource"))

  (let* ((resource-info (kubernetes-utils-get-effective-resource state kubernetes-logs-supported-resource-types)))
    ;; Clear the manual selection after logs are fetched
    (setq kubernetes-utils--selected-resource nil)
    (kubernetes-logs-follow args state)))

;; Function to refresh logs in the current buffer
;;;###autoload
(defun kubernetes-logs-refresh ()
  "Refresh logs in the current buffer."
  (interactive)
  (if (and (derived-mode-p 'kubernetes-logs-mode) (boundp 'kubernetes-logs-kubectl-args))
      (with-current-buffer (kubernetes-utils-process-buffer-start (buffer-name)
                                                                  #'kubernetes-logs-mode
                                                                  kubernetes-kubectl-executable
                                                                  kubernetes-logs-kubectl-args)
        (message "Logs refreshed"))
    (message "Cannot refresh logs: command information not available")))

;; Updated transient definition with context-aware descriptions and resource selection option
(transient-define-prefix kubernetes-logs ()
  "Fetch or tail logs from Kubernetes resources."
  [["Flags"
    ("-a" "Print logs from all containers in this pod" "--all-containers=true")
    ("-p" "Print logs for previous instances of the container in this pod" "-p")
    ("-t" "Include timestamps on each line in the log output" "--timestamps=true")
    ("-A" "Get logs from all pods" "--all-pods=true")
    ("-P" "Prefix each log line with pod name and container name" "--prefix=true")]
   ["Options"
    ("=c" "Select container" "--container=" kubernetes-utils-read-container-for-current-resource)
    ("=t" "Number of lines to display" "--tail=" transient-read-number-N+)]
   ["Time"
    ("=s" "Since relative time" "--since=" kubernetes-utils-read-time-value)
    ("=d" "Since absolute datetime" "--since-time=" kubernetes-utils-read-iso-datetime)]]
  [["Actions"
    ("l" (lambda ()
           (if (kubernetes-utils-has-valid-resource-p kubernetes-logs-supported-resource-types)
               (format "Logs for %s" (kubernetes-utils-get-current-resource-description kubernetes-logs-supported-resource-types))
             (propertize "Logs (no resource selected)" 'face 'transient-inapt-suffix)))
         kubernetes-logs-fetch-all-with-check)
    ("f" (lambda ()
           (if (kubernetes-utils-has-valid-resource-p kubernetes-logs-supported-resource-types)
               (format "Follow logs for %s" (kubernetes-utils-get-current-resource-description kubernetes-logs-supported-resource-types))
             (propertize "Follow logs (no resource selected)" 'face 'transient-inapt-suffix)))
         kubernetes-logs-follow-with-check)
    ("r" "Select resource" kubernetes-logs-select-resource)
    ("b" "Switch logs buffer" kubernetes-logs-switch-buffers)]])

;;;###autoload
(defun kubernetes-logs-reset-and-launch ()
  "Reset the manually selected resource and launch the logs transient menu."
  (interactive)
  (setq kubernetes-utils--selected-resource nil)
  (kubernetes-logs))

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
