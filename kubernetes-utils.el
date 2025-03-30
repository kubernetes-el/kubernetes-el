;; kubernetes-utils.el --- Common utilities.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 'subr-x)
(require 'term)

(require 'kubernetes-ast)
(require 'kubernetes-core)
(require 'kubernetes-kubectl)
(require 'kubernetes-state)
(require 'kubernetes-vars)

(autoload 'org-read-date "org")

(defun kubernetes-utils-read-iso-datetime (&rest _)
  (let* ((date (org-read-date nil t))
         (tz (format-time-string "%z" date)))
    (concat
     (format-time-string "%Y-%m-%dT%H:%M:%S" date)
     (replace-regexp-in-string (rx (group (? (any "+-")) digit digit)
                                   (group digit digit))
                               "\\1:\\2"
                               tz))))

(defun kubernetes-get-pod-container-names (pod)
  "Return the names of all containers available in the specified POD.

Returns nil on invalid input."
  (-let [(&alist 'spec (&alist 'containers containers)) pod]
    (-map (-lambda ((&alist 'name name)) name) containers)))

(define-error 'kubernetes-state-error "Kubernetes state not initialized")


(defun kubernetes-utils-read-time-value (&rest _)
  "Read a relative time value in the style accepted by kubectl.  E.g. 20s, 3h, 5m."
  (let (result)
    (while (null result)
      (let ((input (read-string "Time value (e.g. 20s): ")))
        (if (string-match-p (rx bol (* space) (+ digit) (* space) (any "smh") (* space) eol)
                            input)
            (setq result input)
          (message "Invalid time value")
          (sit-for 1))))
    result))

(defun kubernetes-utils-maybe-pod-name-at-point ()
  "Return the pod name if point is on a pod, nil otherwise."
  (kubernetes-utils-get-resource-name-at-point "pod"))

;; Additional utility functions for detecting resources at point

;;;###autoload
(defun kubernetes-utils-maybe-deployment-name-at-point ()
  "Return the deployment name if point is on a deployment, nil otherwise."
  (kubernetes-utils-get-resource-name-at-point "deployment"))

;;;###autoload
(defun kubernetes-utils-maybe-statefulset-name-at-point ()
  "Return the statefulset name if point is on a statefulset, nil otherwise."
  (kubernetes-utils-get-resource-name-at-point "statefulset"))

;;;###autoload
(defun kubernetes-utils-maybe-job-name-at-point ()
  "Return the job name if point is on a job, nil otherwise."
  (kubernetes-utils-get-resource-name-at-point "job"))

(defalias 'kubernetes-utils-parse-utc-timestamp 'kubernetes--parse-utc-timestamp)

(defun kubernetes-utils-make-cleanup-fn (buf)
  "Make a function to add to `kill-buffer-hook' for a Kubernetes buffer.

BUF is the buffer used to display a Kubernetes feature.  A
reference to it is needed to determine which buffers remain.

The function will terminate polling when the last Kubernetes
buffer is killed."
  (lambda ()
    (let* ((bufs (-keep #'get-buffer (list kubernetes-label-query-buffer-name
                                           kubernetes-overview-buffer-name)))
           (more-buffers (remove buf bufs)))
      (unless more-buffers
        (dolist (b bufs)
          (with-current-buffer b
            (kubernetes-state-clear)))
        (kubernetes-process-kill-polling-processes)
        (kubernetes--kill-timers)))))

(defun kubernetes-utils-term-buffer-start (bufname command args)
  ;; Kill existing process.
  (when-let ((existing (get-buffer bufname))
             (proc (get-buffer-process existing)))
    (kubernetes-process-kill-quietly proc))

  (let ((buf (get-buffer-create bufname)))
    (with-current-buffer buf
      (erase-buffer)
      (buffer-disable-undo)
      (term-mode)
      (goto-char (point-min))
      (let ((time-str (format "Session started at %s" (substring (current-time-string) 0 19)))
            (command-str (format "%s %s" command (string-join args " "))))
        (kubernetes-ast-eval
         `((line ,(propertize time-str 'face 'kubernetes-dimmed))
           (padding)
           (line ,(propertize command-str 'face 'kubernetes-dimmed))
           (padding))))

      (term-exec (current-buffer) "kuberenetes-term" command nil args)
      (let ((proc (get-buffer-process (current-buffer))))
        (set-process-query-on-exit-flag proc nil)
        (term-char-mode)
        (add-hook 'kill-buffer-hook (lambda ()
                                      (when-let (win (get-buffer-window buf))
                                        (quit-window nil win)))
                  nil t)))

    buf))

(defun kubernetes-utils-vterm-start (bufname command args)
  ;; Kill existing process.
  (when-let ((existing (get-buffer bufname)))
    (let ((proc (get-buffer-process existing)))
      (if proc
          (kubernetes-process-kill-quietly proc)
        (kill-buffer bufname))))

  (let* ((vterm-buffer-name bufname)
         (command-str (format "%s %s" command (string-join args " ")))
         (vterm-shell command-str))
    (vterm-other-window)))

(defun kubernetes-utils-process-buffer-start (bufname setup-fn command args &optional process-filter)
  (let ((buf (get-buffer-create bufname)))
    (buffer-disable-undo buf)

    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (funcall setup-fn)
        (let ((time-str (format "Process started at %s" (substring (current-time-string) 0 19)))
              (command-str (format "%s %s" command (string-join args " "))))
          (kubernetes-ast-eval
           `((line ,(propertize time-str 'face 'kubernetes-dimmed))
             (padding)
             (line ,(propertize command-str 'face 'kubernetes-dimmed))
             (padding))))))

    (let ((proc (apply #'start-process "kubernetes-exec" buf command args)))
      (when process-filter
        (set-process-filter proc process-filter))
      (set-process-query-on-exit-flag proc nil))
    buf))

(defun kubernetes-utils-up-to-existing-dir (dir)
  (while (not (file-directory-p dir))
    (setq dir (file-name-directory (directory-file-name dir))))
  dir)

(defun kubernetes-utils-get-resource-info-at-point ()
  "Extract resource type and name from the thing at point.
Return value is either nil or a cons cell of the form (TYPE . NAME),
where TYPE is a string representing the Kubernetes resource type
and NAME is the name of the resource."
  (when-let ((nav-buffer (get-buffer kubernetes-overview-buffer-name)))
    (with-current-buffer nav-buffer
      (when-let ((nav-prop (get-text-property (point) 'kubernetes-nav)))
        (pcase nav-prop
          ;; Extract both resource type and name from the property
          (`(,(and (pred keywordp) resource-key) ,name)
           ;; Convert :resource-type-name to "resource-type"
           (let* ((key-name (substring (symbol-name resource-key) 1)) ; Remove leading ":"
                  (parts (split-string key-name "-name" t))          ; Split on "-name"
                  (resource-type (car parts)))                       ; Get the resource type
             (cons resource-type name))))))))

(defun kubernetes-utils-get-resource-name-at-point (&optional resource-type)
  "Return the name of resource at point.
If RESOURCE-TYPE is provided, only return the name if the resource type matches.
Otherwise, return the name of whatever resource is at point, or nil if not on a resource."
  (when-let ((resource-info (kubernetes-utils-get-resource-info-at-point)))
    (if resource-type
        (when (string= (car resource-info) resource-type)
          (cdr resource-info))
      (cdr resource-info))))

(defun kubernetes-utils--get-container-names-from-pod (pod)
  "Extract container names from POD definition, including all init containers."
  (when pod
    (kubernetes-utils--extract-container-names-from-spec (alist-get 'spec pod))))

(defun kubernetes-utils--get-all-pods-for-owner (owner-type owner-name state)
  "Find all pods for OWNER-TYPE with OWNER-NAME using STATE.
Returns a list of matching pods."
  (let* ((pods-state (alist-get 'pods state))
         (pods (alist-get 'items pods-state)))

    (seq-filter
     (lambda (pod)
       (let-alist pod
         (pcase owner-type
          ("statefulset"
           ;; For statefulsets, find pods with names that follow the pattern
           (string-match-p (concat "^" owner-name "-[0-9]+$") .metadata.name))

          ((or "deployment" "daemonset" "replicaset" "job")
           ;; Generic handler for resources that use ownerReferences
           (let* ((expected-kind (pcase owner-type
                                   ("deployment" "ReplicaSet")
                                   ("daemonset" "DaemonSet")
                                   ("replicaset" "ReplicaSet")
                                   ("job" "Job")))
                  (name-match-fn (if (string= owner-type "deployment")
                                     (lambda (ref-name) (string-match-p owner-name ref-name))
                                   (lambda (ref-name) (equal owner-name ref-name)))))
             (and .metadata.ownerReferences
                  (seq-find
                   (lambda (ref)
                     (and (equal expected-kind (alist-get 'kind ref))
                          (funcall name-match-fn (alist-get 'name ref))))
                   .metadata.ownerReferences))))

          (_ nil))))
     pods)))

(defun kubernetes-utils--extract-container-names-from-spec (spec)
  "Extract container names from resource SPEC.
Works with pod specs and pod template specs."
  (when spec
    (-let [(&alist 'containers containers 'initContainers init-containers) spec]
      (-concat
       (-map (-lambda ((&alist 'name name)) name) (or init-containers '()))
       (-map (-lambda ((&alist 'name name)) name) (or containers '()))))))

(defun kubernetes-utils--get-container-names-sync (resource-type resource-name state)
  "Get container names for RESOURCE-TYPE with RESOURCE-NAME using STATE synchronously.
This function includes support for injected sidecar and init containers."
  (let* ((container-names
          (pcase resource-type
            ("pod"
             ;; For pods, get containers directly from the pod
             (when-let ((pod (kubernetes-state-lookup-pod resource-name state)))
               (kubernetes-utils--get-container-names-from-pod pod)))

            ;; For other resources, extract from template.spec
            ((or "deployment" "statefulset" "daemonset" "replicaset" "job")
             (when-let* ((lookup-fn (intern (format "kubernetes-state-lookup-%s" resource-type)))
                         (resource (funcall lookup-fn resource-name state))
                         (spec (alist-get 'spec resource))
                         (template (alist-get 'template spec))
                         (pod-spec (alist-get 'spec template)))
               (kubernetes-utils--extract-container-names-from-spec pod-spec)))))

         (pod-container-names
          (when (not (string= resource-type "pod"))
            ;; Find all pods for this resource
            (let ((matching-pods (kubernetes-utils--get-all-pods-for-owner resource-type resource-name state)))
              (when matching-pods
                ;; Extract containers from all matching pods and combine them
                (-uniq (-mapcat #'kubernetes-utils--get-container-names-from-pod matching-pods)))))))

    ;; Return the appropriate container names
    (cond
     ((and pod-container-names (> (length pod-container-names) 0))
      pod-container-names)
     ((and container-names (> (length container-names) 0))
      container-names)
     (t '()))))

(defun kubernetes-utils--get-container-names (resource-type resource-name state)
  "Get container names for RESOURCE-TYPE with RESOURCE-NAME using STATE.
Includes support for injected sidecar containers like those from Vault, Istio, etc."
  (kubernetes-utils--get-container-names-sync resource-type resource-name state))

(defun kubernetes-utils-read-container-name (prompt &optional initial-input history)
  "Read a container name from the minibuffer using PROMPT.
If a resource is specified at point, show container names for that resource.
Otherwise, fall back to pod selection.
Optional arguments INITIAL-INPUT and HISTORY are passed to `completing-read'."
  (let* ((state (or (kubernetes-state) (signal 'kubernetes-state-error nil)))
         (resource-info (kubernetes-utils-get-resource-info-at-point))
         (container-names
          (if (and resource-info (member (car resource-info) kubernetes-logs-supported-resource-types))
              ;; Resource found at point - use its type and name
              (let ((resource-type (car resource-info))
                    (resource-name (cdr resource-info)))
                (or (kubernetes-utils--get-container-names resource-type resource-name state)
                    (error "No containers found for %s/%s" resource-type resource-name)))
            ;; No resource at point - fall back to pod selection like original behavior
            (let* ((pod-name (kubernetes-pods--read-name state))
                   (pod (kubernetes-state-lookup-pod pod-name state)))
              (or (kubernetes-utils--get-container-names-from-pod pod)
                  (error "No containers found for pod %s" pod-name))))))
    (if (null container-names)
        (error "No containers available")
      (completing-read (or prompt "Container name: ") container-names nil t initial-input history))))

(defun kubernetes-utils--extract-container-name-from-args (args)
  "Extract container name from ARGS if present. Return nil if not found or ARGS is nil."
  (when args
    (let ((container-arg (seq-find (lambda (arg) (string-prefix-p "--container=" arg)) args)))
      (when container-arg
        (substring container-arg (length "--container="))))))

(defun kubernetes-utils-get-resource-name (state resource-type)
  "Get a resource name of RESOURCE-TYPE using STATE.
If a resource name is available via a specific function, use it.
Otherwise, use a resource-specific read function."
  (let* ((get-resource-name-fn
          (intern-soft (format "kubernetes-utils-maybe-%s-name-at-point"
                               (if (string= resource-type "persistentvolumeclaim")
                                   "pvc"
                                 resource-type))))
         (read-name-fn
          (intern-soft (format "kubernetes-%ss--read-name"
                               (if (string= resource-type "persistentvolumeclaim")
                                   "persistentvolumeclaim"
                                 resource-type))))
         (resource-name (or (when (fboundp get-resource-name-fn)
                              (funcall get-resource-name-fn))
                            (when (and read-name-fn (fboundp read-name-fn))
                              (funcall read-name-fn state)))))
    resource-name))

(defun kubernetes-utils-select-resource (state resource-types)
  "Select a resource from RESOURCE-TYPES using STATE.
Returns a cons cell of (type . name)."
  (interactive)
  (let* ((resource-type (completing-read "Resource type: " resource-types nil t))
         (resource-name (kubernetes-utils-get-resource-name state resource-type)))
    (cons resource-type resource-name)))

(defvar kubernetes-utils--selected-resource nil
  "Currently selected resource for operations, as a cons cell of (type . name).
This variable is used across different operations like logs, exec, and describe.")

(defun kubernetes-utils-clear-selected-resource ()
  "Clear the currently selected resource."
  (setq kubernetes-utils--selected-resource nil))

(defun kubernetes-utils-get-resource-at-point (supported-types)
  "Get the resource at point if it's one of the SUPPORTED-TYPES.
Returns a cons cell of (type . name) or nil if no valid resource at point."
  (when-let ((resource-info (kubernetes-utils-get-resource-info-at-point)))
    (when (member (car resource-info) supported-types)
      resource-info)))

(defun kubernetes-utils-has-valid-resource-p (supported-types)
  "Return non-nil if there is a valid resource for operations.
Either a resource at point in SUPPORTED-TYPES or a manually selected resource."
  (or (kubernetes-utils-get-resource-at-point supported-types)
      kubernetes-utils--selected-resource))

(defun kubernetes-utils-get-resource-description (resource-info)
  "Get a descriptive string for RESOURCE-INFO (a cons of type and name).
Returns a string like 'pod/nginx' or 'deployment/web'."
  (when resource-info
    (format "%s/%s" (car resource-info) (cdr resource-info))))

(defun kubernetes-utils-get-current-resource-description (supported-types)
  "Get a descriptive string for the current resource.
Uses manually selected resource if available, otherwise shows resource at point.
SUPPORTED-TYPES is a list of resource types that are valid for the operation."
  (if kubernetes-utils--selected-resource
      (kubernetes-utils-get-resource-description kubernetes-utils--selected-resource)
    (if-let ((resource-at-point (kubernetes-utils-get-resource-at-point supported-types)))
        (kubernetes-utils-get-resource-description resource-at-point)
      "selected resource")))

(defun kubernetes-utils-read-resource-if-needed (state supported-types &optional fallback-type)
  "Read a resource from the minibuffer if none is at point using STATE.
SUPPORTED-TYPES is a list of resource types valid for the operation.
If FALLBACK-TYPE is provided, it will be used for fallback selection.
Returns a cons cell of (type . name)."
  (or kubernetes-utils--selected-resource
      (kubernetes-utils-get-resource-at-point supported-types)
      (let ((fallback (or fallback-type (car supported-types))))
        (cons fallback (kubernetes-utils-get-resource-name state fallback)))))

(defun kubernetes-utils-get-effective-resource (state supported-types)
  "Get the resource to use for operations.
Uses manually selected resource if available, otherwise uses resource at point.
If neither is available, prompts the user.
STATE is the current application state.
SUPPORTED-TYPES is a list of resource types valid for the operation.
Returns a cons cell of (type . name)."
  (or kubernetes-utils--selected-resource
      (kubernetes-utils-get-resource-at-point supported-types)
      (let ((selected (kubernetes-utils-select-resource state supported-types)))
        ;; Store the selection for current use
        (setq kubernetes-utils--selected-resource selected)
        selected)))


(defun kubernetes-utils-generate-operation-buffer-name (operation resource-type resource-name args
                                                      &optional state alt-format)
  "Generate a buffer name for OPERATION on RESOURCE-TYPE named RESOURCE-NAME.
OPERATION is a string like 'logs', 'exec term', or 'describe'.
ARGS are additional args that might contain container info.
STATE is used to get namespace information when available.
If ALT-FORMAT is non-nil, an alternative format will be used (e.g., for vterm buffers).
Returns a formatted buffer name string."
  (let* ((container-name (kubernetes-utils--extract-container-name-from-args args))
         (container-suffix (if container-name (format ":%s" container-name) ""))
         (namespace (when state (kubernetes-state--get state 'current-namespace)))
         (namespace-prefix (if namespace (format "%s/" namespace) "")))
    (format "*kubernetes %s: %s%s/%s%s*"
            operation
            namespace-prefix
            resource-type
            resource-name
            container-suffix)))

(defun kubernetes-utils-read-container-for-resource (prompt state resource-type resource-name
                                                    &optional initial-input history)
  "Read a container name for a specific resource.
PROMPT is displayed when requesting container name input.
STATE is the current Kubernetes state.
RESOURCE-TYPE and RESOURCE-NAME identify the resource with containers.
INITIAL-INPUT is the initial value for the prompt.
HISTORY is the history list to use."
  (let ((container-names (kubernetes-utils--get-container-names
                          resource-type resource-name state)))
    (if (null container-names)
        (error "No containers found for %s/%s" resource-type resource-name)
      (completing-read (or prompt "Container name: ")
                       container-names nil t initial-input history))))

(defun kubernetes-utils-read-container-for-current-resource (prompt initial-input history)
  "Read a container name for the current resource (selected or at point).
PROMPT is displayed when requesting container name input.
INITIAL-INPUT is the initial value for the prompt.
HISTORY is the history list to use."
  (let* ((state (kubernetes-state))
         (resource-info (kubernetes-utils-get-effective-resource
                         state (append kubernetes-logs-supported-resource-types
                                     kubernetes-exec-supported-resource-types))))
    (kubernetes-utils-read-container-for-resource
     prompt state (car resource-info) (cdr resource-info) initial-input history)))

(provide 'kubernetes-utils)

;;; kubernetes-utils.el ends here
