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
(defvar kubernetes-exec-buffer-name-format "*kubernetes exec: %s%s/%s%s*"
  "Format for Kubernetes exec buffer names.
First %s is replaced with the namespace prefix if specified (including trailing slash),
second %s is the resource type,
third %s is the resource name,
fourth %s is replaced with container suffix if specified (including the leading colon).")

(defvar kubernetes-exec-vterm-buffer-name-format "*kubernetes vterm exec: %s%s/%s%s*"
  "Format for Kubernetes exec buffer names using vterm.
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

(defun kubernetes-exec--read-resource-if-needed (state)
  "Read a resource from the minibuffer if none is at point using STATE.
Returns a cons cell of (type . name)."
  (or (when-let ((resource-info (kubernetes-utils-get-resource-info-at-point)))
        (when (member (car resource-info) kubernetes-exec-supported-resource-types)
          resource-info))
      ;; No execable resource at point, default to pod selection
      (cons "pod" (kubernetes-pods--read-name state))))

(defun kubernetes-exec--generate-buffer-name (resource-type resource-name args &optional state use-vterm)
  "Generate a buffer name for exec into RESOURCE-TYPE named RESOURCE-NAME with ARGS.
STATE is the optional current application state for namespace info.
If USE-VTERM is non-nil, use vterm buffer name format."
  (let* ((container-arg (seq-find (lambda (arg) (string-prefix-p "--container=" arg)) args))
         (container-name (when container-arg
                           (substring container-arg (length "--container="))))
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
  (let* ((resource-path (if (string= resource-type "pod")
                          resource-name
                        (format "%s/%s" resource-type resource-name)))
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
      (setq-local kubernetes-exec-container-name
                  (let ((container-arg (seq-find (lambda (arg) (string-prefix-p "--container=" arg)) args)))
                    (when container-arg
                      (substring container-arg (length "--container="))))))

    ;; Setup process cleanup if needed
    (when (and interactive-tty kubernetes-clean-up-interactive-exec-buffers)
      (set-process-sentinel (get-buffer-process buf) #'kubernetes-process-kill-quietly))

    (select-window (display-buffer buf))))

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

;;;###autoload
(defun kubernetes-exec-list-buffers ()
  "List all Kubernetes exec buffers and allow selecting one."
  (interactive)
  (let ((exec-buffers nil))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and (or (string-prefix-p "*kubernetes exec" (buffer-name))
                      (string-prefix-p "*kubernetes vterm exec" (buffer-name)))
                   (boundp 'kubernetes-exec-resource-type))
          (push buffer exec-buffers))))

    (if exec-buffers
        (let* ((buffer-desc-alist
                (mapcar (lambda (buf)
                          (with-current-buffer buf
                            (let* ((desc (if (and (boundp 'kubernetes-exec-resource-type)
                                                 (boundp 'kubernetes-exec-resource-name))
                                            (format "%s%s/%s%s: %s"
                                                    (if (and (boundp 'kubernetes-exec-namespace)
                                                             kubernetes-exec-namespace)
                                                        (format "%s/" kubernetes-exec-namespace)
                                                      "")
                                                    kubernetes-exec-resource-type
                                                    kubernetes-exec-resource-name
                                                    (if (and (boundp 'kubernetes-exec-container-name)
                                                             kubernetes-exec-container-name)
                                                        (format " (container: %s)"
                                                               kubernetes-exec-container-name)
                                                      "")
                                                    (if (boundp 'kubernetes-exec-command)
                                                        kubernetes-exec-command
                                                      ""))
                                          (buffer-name))))
                              (cons desc buf))))
                        exec-buffers))
               (selected-desc (completing-read "Select exec buffer: "
                                              (mapcar #'car buffer-desc-alist)
                                              nil t))
               (selected-buffer (cdr (assoc selected-desc buffer-desc-alist))))
          (switch-to-buffer selected-buffer))
      (message "No Kubernetes exec buffers found"))))

;; Use existing transient command definition
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
     :inapt-if-not (lambda () (require 'vterm nil 'noerror)))
    ("l" "List exec buffers" kubernetes-exec-list-buffers)]])

(provide 'kubernetes-exec)

;;; kubernetes-exec.el ends here
