;;; kubernetes-exec.el --- Support for executing commands in Kubernetes resources  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 's)
(require 'transient)

(require 'kubernetes-ast)
(require 'kubernetes-core)
(require 'kubernetes-kubectl)
(require 'kubernetes-modes)
(require 'kubernetes-pods)
(require 'kubernetes-process)
(require 'kubernetes-state)
(require 'kubernetes-utils)
(require 'kubernetes-vars)

(defconst kubernetes-exec-supported-resource-types
  '("pod" "deployment" "statefulset" "job" "cronjob")
  "List of Kubernetes resource types that support exec functionality.")

;; Define buffer name format similar to logs
(defvar kubernetes-exec-buffer-name-format "*kubernetes exec term: %s%s/%s%s*"
  "Format for Kubernetes exec buffers using term-mode.
First %s is replaced with the namespace prefix if specified (including trailing slash),
second %s is the resource type,
third %s is the resource name,
fourth %s is replaced with container suffix if specified (including the leading colon).")

(defvar kubernetes-exec-vterm-buffer-name-format "*kubernetes exec vterm: %s%s/%s%s*"
  "Format for Kubernetes exec buffers using vterm.
First %s is replaced with the namespace prefix if specified (including trailing slash),
second %s is the resource type,
third %s is the resource name,
fourth %s is replaced with container suffix if specified (including the leading colon).")

;; Customizable options
(defcustom kubernetes-clean-up-interactive-exec-buffers nil
  "If non-nil, automatically kill interactive exec buffers when the process ends.
Otherwise, the buffers are kept alive to allow reviewing the command output."
  :group 'kubernetes
  :type 'boolean)

;; Store the manually selected resource for exec
(defvar kubernetes-exec--selected-resource nil
  "Currently selected resource for exec, as a cons cell of (type . name).")

(defun kubernetes-exec--get-resource-at-point ()
  "Get the resource at point if it's a supported resource type.
Returns a cons cell of (type . name) or nil if no valid resource at point."
  (when-let ((resource-info (kubernetes-utils-get-resource-info-at-point)))
    (when (member (car resource-info) kubernetes-exec-supported-resource-types)
      resource-info)))

(defun kubernetes-exec--has-valid-resource-p ()
  "Return non-nil if there is a valid resource for exec.
Either a resource at point or a manually selected resource."
  (or (kubernetes-exec--get-resource-at-point)
      kubernetes-exec--selected-resource))

(defun kubernetes-exec--get-current-resource-description ()
  "Get a descriptive string for the current resource.
Uses manually selected resource if available, otherwise shows resource at point."
  (if kubernetes-exec--selected-resource
      (format "%s/%s"
              (car kubernetes-exec--selected-resource)
              (cdr kubernetes-exec--selected-resource))
    (if-let ((resource-at-point (kubernetes-exec--get-resource-at-point)))
        (format "%s/%s" (car resource-at-point) (cdr resource-at-point))
      "selected resource")))

(defun kubernetes-exec--read-resource-if-needed (state)
  "Read a resource from the minibuffer if none is at point using STATE.
Returns a cons cell of (type . name)."
  (or (kubernetes-exec--get-resource-at-point)
      kubernetes-exec--selected-resource
      (cons "pod" (kubernetes-pods--read-name state))))

(defun kubernetes-exec--get-effective-resource (state)
  "Get the resource to use for exec operations.
Uses manually selected resource if available, otherwise uses resource at point.
If neither is available, prompts the user.
STATE is the current application state."
  (or kubernetes-exec--selected-resource
      (kubernetes-exec--get-resource-at-point)
      (let ((selected (kubernetes-utils-select-resource state kubernetes-exec-supported-resource-types)))
        ;; Store the selection for current use
        (setq kubernetes-exec--selected-resource selected)
        selected)))

(defun kubernetes-exec-select-resource ()
  "Select a resource for exec and store it for use in the transient."
  (interactive)
  (let* ((state (kubernetes-state))
         (resource-info nil))
    ;; Use condition-case to catch keyboard quit
    (condition-case nil
        (setq resource-info (kubernetes-utils-select-resource state kubernetes-exec-supported-resource-types))
      (quit (setq kubernetes-exec--selected-resource nil)
            (error "Selection canceled")))

    (when resource-info
      (setq kubernetes-exec--selected-resource resource-info)
      (message "Selected %s/%s for exec" (car resource-info) (cdr resource-info))
      (transient-setup 'kubernetes-exec))))

(defun kubernetes-exec--generate-buffer-name (resource-type resource-name args &optional state use-vterm)
  "Generate a buffer name for exec into RESOURCE-TYPE named RESOURCE-NAME with ARGS.
STATE is the optional current application state for namespace info.
If USE-VTERM is non-nil, use vterm buffer name format."
  (let* ((container-name (kubernetes-utils--extract-container-name-from-args args))
         (container-suffix (if container-name (format ":%s" container-name) ""))
         (namespace (when state (kubernetes-state--get state 'current-namespace)))
         (namespace-prefix (if namespace (format "%s/" namespace) ""))
         (format-string (if use-vterm
                           kubernetes-exec-vterm-buffer-name-format
                         kubernetes-exec-buffer-name-format)))
    (format format-string
            namespace-prefix
            resource-type
            resource-name
            container-suffix)))

;;;###autoload
(defun kubernetes-exec-into (pod-name args exec-command state)
  "Open a terminal for executing into a pod or other resource.

If POD-NAME is actually prefixed with a resource type (e.g. deployment/my-deploy),
it will be handled appropriately.

ARGS are additional args to pass to kubectl.
EXEC-COMMAND is the command to run in the container.
STATE is the current application state."
  (interactive (let* ((state (kubernetes-state))
                      (resource-info (kubernetes-exec--read-resource-if-needed state))
                      (resource-type (car resource-info))
                      (resource-name (cdr resource-info))
                      (resource-path (if (string= resource-type "pod")
                                        resource-name
                                      (format "%s/%s" resource-type resource-name)))
                      (command
                       (let ((cmd (string-trim (read-string
                                               (format "Command (default: %s): "
                                                      kubernetes-default-exec-command)
                                               nil 'kubernetes-exec-history))))
                         (if (string-empty-p cmd)
                             kubernetes-default-exec-command
                           cmd))))
                 (list resource-path (transient-args 'kubernetes-exec) command state)))

  ;; Handle different resource formats
  (if (string-match "\\([^/]+\\)/\\(.+\\)" pod-name)
      ;; Resource format like "deployment/my-deployment"
      (let ((resource-type (match-string 1 pod-name))
            (resource-name (match-string 2 pod-name)))
        (kubernetes-exec--exec-internal resource-type resource-name args exec-command state))
    ;; Standard pod name
    (kubernetes-exec--exec-internal "pod" pod-name args exec-command state)))

(defun kubernetes-exec--exec-internal (resource-type resource-name args exec-command state)
  "Execute a command on a Kubernetes resource using kubectl exec.

RESOURCE-TYPE is the type of resource (pod, deployment, etc.).
RESOURCE-NAME is the name of the resource to execute on.
ARGS are additional args to pass to kubectl.
EXEC-COMMAND is the command to run in the container.
STATE is the current application state."
  ;; Build the command arguments for kubectl exec
  (let* ((resource-path (format "%s/%s" resource-type resource-name))
         (command-args (append (list "exec")
                              (kubernetes-kubectl--flags-from-state state)
                              args
                              (when-let (ns (kubernetes-state--get state 'current-namespace))
                                (list (format "--namespace=%s" ns)))
                              (list resource-path "--" exec-command)))
         (interactive-tty (member "--tty" args))
         (buffer-name (kubernetes-exec--generate-buffer-name
                       resource-type resource-name args state))
         (buf nil))

    ;; Clear the selected resource after use
    (setq kubernetes-exec--selected-resource nil)

    ;; Choose the appropriate execution method
    (setq buf
          (if interactive-tty
              (kubernetes-utils-term-buffer-start buffer-name
                                                kubernetes-kubectl-executable
                                                command-args)
            (kubernetes-utils-process-buffer-start buffer-name
                                                 #'kubernetes-mode
                                                 kubernetes-kubectl-executable
                                                 command-args)))

    ;; Store command information in buffer-local variables for later use
    (with-current-buffer buf
      (setq-local kubernetes-exec-resource-type resource-type)
      (setq-local kubernetes-exec-resource-name resource-name)
      (setq-local kubernetes-exec-command exec-command)
      (setq-local kubernetes-exec-kubectl-args command-args)
      (setq-local kubernetes-exec-namespace (kubernetes-state--get state 'current-namespace))
      (setq-local kubernetes-exec-container-name (kubernetes-utils--extract-container-name-from-args args)))

    ;; Setup process cleanup if needed
    (when (and interactive-tty kubernetes-clean-up-interactive-exec-buffers)
      (set-process-sentinel (get-buffer-process buf) #'kubernetes-process-kill-quietly))

    (select-window (display-buffer buf))))

(defun kubernetes-exec--read-container-for-selected-resource (prompt initial-input history)
  "Read a container name for the selected resource.
PROMPT is displayed when requesting container name input.
INITIAL-INPUT is the initial value for the prompt.
HISTORY is the history list to use."
  (let* ((state (kubernetes-state))
         (resource-info (kubernetes-exec--get-effective-resource state))
         (resource-type (car resource-info))
         (resource-name (cdr resource-info))
         (container-names (kubernetes-utils--get-container-names resource-type resource-name state)))
    (if (null container-names)
        (error "No containers found for %s/%s" resource-type resource-name)
      (completing-read (or prompt "Container name: ") container-names nil t initial-input history))))

;;;###autoload
(defun kubernetes-exec-using-vterm (pod-name args exec-command state)
  "Open a vterm terminal for exec into a pod or other resource.

POD-NAME is the name of the pod to exec into or a resource/name combination.
ARGS are additional args to pass to kubectl.
EXEC-COMMAND is the command to run in the container.
STATE is the current application state."
  (interactive (let* ((state (kubernetes-state))
                      (resource-info (kubernetes-exec--read-resource-if-needed state))
                      (resource-type (car resource-info))
                      (resource-name (cdr resource-info))
                      (resource-path (if (string= resource-type "pod")
                                        resource-name
                                      (format "%s/%s" resource-type resource-name)))
                      (command
                       (let ((cmd (string-trim (read-string
                                               (format "Command (default: %s): "
                                                      kubernetes-default-exec-command)
                                               nil 'kubernetes-exec-history))))
                         (if (string-empty-p cmd)
                             kubernetes-default-exec-command
                           cmd))))
                 (list resource-path (transient-args 'kubernetes-exec) command state)))

  (unless (require 'vterm nil 'noerror)
    (error "This action requires the vterm package"))

  ;; Handle different resource formats
  (let ((resource-type "pod")
        (resource-name pod-name))

    ;; Parse "type/name" format if present
    (when (string-match "\\([^/]+\\)/\\(.+\\)" pod-name)
      (setq resource-type (match-string 1 pod-name))
      (setq resource-name (match-string 2 pod-name)))

    ;; Clear the selected resource after use
    (setq kubernetes-exec--selected-resource nil)

    ;; Build command args
    (let* ((resource-path (if (string= resource-type "pod")
                            resource-name
                          (format "%s/%s" resource-type resource-name)))
           (command-args (append (list "exec")
                                (kubernetes-kubectl--flags-from-state state)
                                args
                                (when-let (ns (kubernetes-state--get state 'current-namespace))
                                  (list (format "--namespace=%s" ns)))
                                (list resource-path "--" exec-command)))
           (buffer-name (kubernetes-exec--generate-buffer-name
                        resource-type resource-name args state t)))

      ;; Start vterm with command
      (kubernetes-utils-vterm-start buffer-name
                                   kubernetes-kubectl-executable
                                   command-args))))

;; Wrapper functions for the transient interface to check resource validity
(defun kubernetes-exec-into-with-check (args state)
  "Call `kubernetes-exec-into' if a resource is available.
ARGS and STATE are passed to the function."
  (interactive
   (list (transient-args 'kubernetes-exec)
         (kubernetes-state)))
  (unless (kubernetes-exec--has-valid-resource-p)
    (user-error "No resource selected. Press 'r' to select a resource"))

  (let* ((state (kubernetes-state))
         (resource-info (kubernetes-exec--get-effective-resource state))
         (resource-type (car resource-info))
         (resource-name (cdr resource-info))
         (resource-path (if (string= resource-type "pod")
                          resource-name
                        (format "%s/%s" resource-type resource-name)))
         (command
          (let ((cmd (string-trim (read-string
                                  (format "Command (default: %s): "
                                         kubernetes-default-exec-command)
                                  nil 'kubernetes-exec-history))))
            (if (string-empty-p cmd)
                kubernetes-default-exec-command
              cmd))))
    (kubernetes-exec-into resource-path args command state)))

(defun kubernetes-exec-vterm-with-check (args state)
  "Call `kubernetes-exec-using-vterm' if a resource is available.
ARGS and STATE are passed to the function."
  (interactive
   (list (transient-args 'kubernetes-exec)
         (kubernetes-state)))
  (unless (kubernetes-exec--has-valid-resource-p)
    (user-error "No resource selected. Press 'r' to select a resource"))

  (let* ((state (kubernetes-state))
         (resource-info (kubernetes-exec--get-effective-resource state))
         (resource-type (car resource-info))
         (resource-name (cdr resource-info))
         (resource-path (if (string= resource-type "pod")
                          resource-name
                        (format "%s/%s" resource-type resource-name)))
         (command
          (let ((cmd (string-trim (read-string
                                  (format "Command (default: %s): "
                                         kubernetes-default-exec-command)
                                  nil 'kubernetes-exec-history))))
            (if (string-empty-p cmd)
                kubernetes-default-exec-command
              cmd))))
    (kubernetes-exec-using-vterm resource-path args command state)))

;;;###autoload
(defun kubernetes-exec-switch-buffers ()
  "List all Kubernetes exec buffers and allow selecting one."
  (interactive)
  ;; Use manual filtering instead of seq-filter
  (let ((exec-buffers nil))
    (dolist (buf (buffer-list))
      (let ((name (buffer-name buf)))
        (when (or (string-prefix-p "*kubernetes exec term" name)
                  (string-prefix-p "*kubernetes exec vterm" name))
          (push buf exec-buffers))))
    (if (null exec-buffers)
        (message "No Kubernetes exec buffers found")
      ;; Use completing-read with buffer category annotation for embark
      (let* ((buffer-names (mapcar 'buffer-name exec-buffers))
             (selected-name (completing-read
                            "Select exec buffer: "
                            (lambda (string pred action)
                              (if (eq action 'metadata)
                                  '(metadata (category . buffer))
                                (complete-with-action
                                 action buffer-names string pred))))))
        (when selected-name
          (switch-to-buffer (get-buffer selected-name)))))))

;;;###autoload
(defun kubernetes-exec-reset-and-launch ()
  "Reset the manually selected resource and launch the logs transient menu."
  (interactive)
  (setq kubernetes-exec--selected-resource nil)
  (kubernetes-exec))

;; Use existing transient command definition
(transient-define-prefix kubernetes-exec ()
  "Execute into Kubernetes resources."
  :value '("--stdin" "--tty")
  ["Switches"
   ("-i" "Pass stdin to container" "--stdin")
   ("-t" "Stdin is a TTY" "--tty")]
  ["Options"
   ("=c" "Select container" "--container=" kubernetes-exec--read-container-for-selected-resource)]
  [["Actions"
    ("e" (lambda ()
           (if (kubernetes-exec--has-valid-resource-p)
               (format "Exec into %s" (kubernetes-exec--get-current-resource-description))
             (propertize "Exec (no resource selected)" 'face 'transient-inapt-suffix)))
     kubernetes-exec-into-with-check)
    ("v" (lambda ()
           (if (and (kubernetes-exec--has-valid-resource-p)
                   (require 'vterm nil 'noerror))
               (format "Exec into %s using vterm" (kubernetes-exec--get-current-resource-description))
             (propertize "Exec using vterm (no resource selected)" 'face 'transient-inapt-suffix)))
     kubernetes-exec-vterm-with-check
     :inapt-if-not (lambda () (require 'vterm nil 'noerror)))
    ("r" "Select resource" kubernetes-exec-select-resource)
    ("b" "Switch exec buffer" kubernetes-exec-switch-buffers)]])

(provide 'kubernetes-exec)

;;; kubernetes-exec.el ends here
