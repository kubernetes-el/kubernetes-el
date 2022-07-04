;;; kubernetes-commands.el --- Interactive commands for Kubernetes modes.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 's)

(require 'kubernetes-ast)
(require 'kubernetes-contexts)
(require 'kubernetes-core)
(require 'kubernetes-modes)
(require 'kubernetes-pods)
(require 'kubernetes-popups)
(require 'kubernetes-state)
(require 'kubernetes-utils)
(require 'mode-local)

(autoload 'kubernetes-configmaps-delete-marked "kubernetes-configmaps")
(autoload 'kubernetes-deployments-delete-marked "kubernetes-deployments")
(autoload 'kubernetes-statefulsets-delete-marked "kubernetes-statefulsets")
(autoload 'kubernetes-ingress-delete-marked "kubernetes-ingress")
(autoload 'kubernetes-display-config "kubernetes-contexts")
(autoload 'kubernetes-display-configmap "kubernetes-configmaps")
(autoload 'kubernetes-display-deployment "kubernetes-deployments")
(autoload 'kubernetes-display-statefulset "kubernetes-statefulsets")
(autoload 'kubernetes-display-ingress "kubernetes-ingress")
(autoload 'kubernetes-display-node "kubernetes-nodes")
(autoload 'kubernetes-display-job "kubernetes-jobs")
(autoload 'kubernetes-display-namespace "kubernetes-namespaces")
(autoload 'kubernetes-display-persistentvolumeclaim "kubernetes-persistentvolumeclaims")
(autoload 'kubernetes-display-pod "kubernetes-pods")
(autoload 'kubernetes-display-secret "kubernetes-secrets")
(autoload 'kubernetes-display-service "kubernetes-services")
(autoload 'kubernetes-jobs-delete-marked "kubernetes-jobs")
(autoload 'kubernetes-persistentvolumeclaims-delete-marked "kubernetes-persistentvolumeclaims")
(autoload 'kubernetes-pods-delete-marked "kubernetes-pods")
(autoload 'kubernetes-secrets-delete-marked "kubernetes-secrets")
(autoload 'kubernetes-services-delete-marked "kubernetes-services")
(autoload 'kubernetes-show-pods-for-label "kubernetes-labels")


;; Mark management

;;;###autoload
(defun kubernetes-mark-for-delete (point)
  "Mark the thing at POINT for deletion, then advance to the next line."
  (interactive "d")
  (pcase (get-text-property point 'kubernetes-nav)
    (`(:pod-name ,name)
     (kubernetes-state-mark-pod name))
    (`(:configmap-name ,name)
     (kubernetes-state-mark-configmap name))
    (`(:ingress-name ,name)
     (kubernetes-state-mark-ingress name))
    (`(:job-name ,name)
     (kubernetes-state-mark-job name))
    (`(:secret-name ,name)
     (kubernetes-state-mark-secret name))
    (`(:service-name ,name)
     (kubernetes-state-mark-service name))
    (`(:deployment-name ,name)
     (kubernetes-state-mark-deployment name))
    (`(:statefulset-name ,name)
     (kubernetes-state-mark-statefulset name))
    (`(:persistentvolumeclaim-name ,name)
     (kubernetes-state-mark-persistentvolumeclaim name))
    (_
     (user-error "Nothing here can be marked")))

  (let ((inhibit-read-only t))
    (kubernetes-ast-put-delete-mark-on-line-at-pt point))
  (magit-section-forward))

;;;###autoload
(defun kubernetes-unmark (point)
  "Unmark the thing at POINT, then advance to the next line."
  (interactive "d")
  (pcase (get-text-property point 'kubernetes-nav)
    (`(:pod-name ,name)
     (kubernetes-state-unmark-pod name))
    (`(:configmap-name ,name)
     (kubernetes-state-unmark-configmap name))
    (`(:ingress-name ,name)
     (kubernetes-state-unmark-ingress name))
    (`(:job-name ,name)
     (kubernetes-state-unmark-job name))
    (`(:secret-name ,name)
     (kubernetes-state-unmark-secret name))
    (`(:service-name ,name)
     (kubernetes-state-unmark-service name))
    (`(:deployment-name ,name)
     (kubernetes-state-unmark-deployment name))
    (`(:statefulset-name ,name)
     (kubernetes-state-unmark-statefulset name))
    (`(:persistentvolumeclaim-name ,name)
     (kubernetes-state-unmark-persistentvolumeclaim name)))
  (kubernetes-state-trigger-redraw)
  (goto-char point)
  (magit-section-forward))

;;;###autoload
(defun kubernetes-unmark-all ()
  "Unmark everything in the buffer."
  (interactive)
  (kubernetes-state-unmark-all)
  (let ((pt (point)))
    (kubernetes-state-trigger-redraw)
    (goto-char pt)))

;;;###autoload
(defun kubernetes-execute-marks ()
  "Action all marked items in the buffer."
  (interactive)
  (let ((state (kubernetes-state)))
    (let ((n (length (kubernetes-state--get state 'marked-pods))))
      (when (and (not (zerop n))
                 (y-or-n-p (format "Delete %s pod%s? " n (if (equal 1 n) "" "s"))))
        (kubernetes-pods-delete-marked state)))

    (let ((n (length (kubernetes-state--get state 'marked-configmaps))))
      (when (and (not (zerop n))
                 (y-or-n-p (format "Delete %s configmap%s? " n (if (equal 1 n) "" "s"))))
        (kubernetes-configmaps-delete-marked state)))

    (let ((n (length (kubernetes-state--get state 'marked-ingress))))
      (when (and (not (zerop n))
                 (y-or-n-p (format "Delete %s ingress%s? " n (if (equal 1 n) "" "s"))))
        (kubernetes-ingress-delete-marked state)))

    (let ((n (length (kubernetes-state--get state 'marked-secrets))))
      (when (and (not (zerop n))
                 (y-or-n-p (format "Delete %s secret%s? " n (if (equal 1 n) "" "s"))))
        (kubernetes-secrets-delete-marked state)))

    (let ((n (length (kubernetes-state--get state 'marked-deployments))))
      (when (and (not (zerop n))
                 (y-or-n-p (format "Delete %s deployment%s? " n (if (equal 1 n) "" "s"))))
        (kubernetes-deployments-delete-marked state)))

    (let ((n (length (kubernetes-state--get state 'marked-statefulsets))))
      (when (and (not (zerop n))
                 (y-or-n-p (format "Delete %s statefulsets%s? " n (if (equal 1 n) "" "s"))))
        (kubernetes-statefulsets-delete-marked state)))

    (let ((n (length (kubernetes-state--get state 'marked-jobs))))
      (when (and (not (zerop n))
                 (y-or-n-p (format "Delete %s job%s? " n (if (equal 1 n) "" "s"))))
        (kubernetes-jobs-delete-marked state)))

    (let ((n (length (kubernetes-state--get state 'marked-services))))
      (when (and (not (zerop n))
                 (y-or-n-p (format "Delete %s service%s? " n (if (equal 1 n) "" "s"))))
        (kubernetes-services-delete-marked state)))

    (let ((n (length (kubernetes-state--get state 'marked-persistentvolumeclaims))))
      (when (and (not (zerop n))
                 (y-or-n-p (format "Delete %s PVC%s? " n (if (equal 1 n) "" "s"))))
        (kubernetes-persistentvolumeclaims-delete-marked state))))

  (kubernetes-unmark-all))


;; Misc commands

;;;###autoload
(defun kubernetes-kill-buffers (&optional no-confirm)
  "Kill all `kubernetes-mode' buffers.

With prefix argument NO-CONFIRM, skips confirmation prior to killing all
buffers."
  (interactive "P")
  (let* ((kubernetes-buffer-p
          (lambda (buffer)
            (let ((major-mode (buffer-local-value 'major-mode buffer)))
              (or (eq major-mode 'kubernetes-mode)
                  (eq (get-mode-local-parent major-mode) 'kubernetes-mode)))))
         (buffers (-filter kubernetes-buffer-p (buffer-list)))
         (num-buffers (length buffers)))
    (if (not buffers)
        (message "No Kubernetes buffers to kill.")
      (when (or no-confirm (y-or-n-p (format "Kill %s Kubernetes buffer(s)? " num-buffers)))
        (dolist (buffer buffers)
          (kill-buffer buffer))
        (kubernetes--info "Killed %s Kubernetes buffers." num-buffers)))))

(defun kubernetes-kill-buffers-and-processes (&optional no-confirm)
  "Kill all kubernetes-el buffers and all processes.

With prefix argument NO-CONFIRM, skips confirmation prior to kill all buffers."
  (interactive "P")
  (kubernetes-kill-buffers no-confirm)
  (release-all kubernetes--global-process-ledger))

;;;###autoload
(defun kubernetes-copy-thing-at-point (point)
  "Perform a context-sensitive copy action.

Inspects the `kubernetes-copy' text property at POINT to determine
what to copy."
  (interactive "d")
  (when-let (s (get-text-property point 'kubernetes-copy))
    (kill-new s)

    ;; Print a user-friendly message for feedback.
    (let ((n-lines 1) (first-line nil))
      (with-temp-buffer
        (insert s)
        (goto-char (point-min))
        (setq first-line (buffer-substring (line-beginning-position) (line-end-position)))
        (while (search-forward "\n" nil t)
          (setq n-lines (1+ n-lines))))
      (let ((ellipsized (s-truncate 70 first-line)))
        (if (< 1 n-lines)
            (message "Copied %s lines, starting with: %s" n-lines ellipsized)
          (message "Copied: %s" ellipsized))))))

;;;###autoload
(defun kubernetes-refresh (&optional verbose)
  "Force Kubernetes buffers to redraw.

With optional argument VERBOSE, log status changes."
  (interactive "p")
  (run-hook-with-args 'kubernetes-poll-hook verbose)
  (kubernetes-state-trigger-redraw))

;;;###autoload
(defun kubernetes-navigate (point state)
  "Perform a context-sensitive navigation action.

STATE is the current application state.

Inspecs the `kubernetes-nav' text property at POINT to determine
how to navigate.  If that property is not found, attempt to toggle
the magit section at point."
  (interactive (list (point) (kubernetes-state)))
  (pcase (get-text-property point 'kubernetes-nav)
    (:display-config
     (kubernetes-display-config (alist-get 'config state)))
    (`(:configmap-name ,configmap-name)
     (kubernetes-display-configmap configmap-name state))
    (`(:service-name ,service-name)
     (kubernetes-display-service service-name state))
    (`(:deployment-name ,deployment-name)
     (kubernetes-display-deployment deployment-name state))
    (`(:statefulset-name ,statefulset-name)
     (kubernetes-display-statefulset statefulset-name state))
    (`(:ingress-name ,ingress-name)
     (kubernetes-display-ingress ingress-name state))
    (`(:node-name ,node-name)
     (kubernetes-display-node node-name state))
    (`(:job-name ,job-name)
     (kubernetes-display-job job-name state))
    (`(:secret-name ,secret-name)
     (kubernetes-display-secret secret-name state))
    (`(:namespace-name ,namespace-name)
     (kubernetes-display-namespace namespace-name state))
    (`(:pod-name ,pod-name)
     (kubernetes-display-pod pod-name state))
    (`(:persistentvolumeclaim-name ,persistentvolumeclaim-name)
     (kubernetes-display-persistentvolumeclaim persistentvolumeclaim-name state))
    (`(:selector ,selector)
     (kubernetes-show-pods-for-label selector))
    (_
     (when-let (section (get-text-property (point) 'magit-section))
       (magit-section-toggle section)))))

(defun kubernetes--describable-thing-at-pt ()
  (save-excursion
    (back-to-indentation)
    (get-text-property (point) 'kubernetes-nav)))

;;;###autoload
(defun kubernetes-describe-dwim (thing)
  "Describe the thing at point.

THING must be a valid target for `kubectl describe'."
  (interactive (list (kubernetes--describable-thing-at-pt)))
  (pcase thing
    (`(:pod-name ,pod-name)
     (kubernetes-describe-pod pod-name))
    (_
     (user-error "Nothing at point to describe"))))

;;;###autoload
(defun kubernetes-describe-pod (pod-name)
  "Display a buffer for describing a pod.

POD-NAME is the name of the pod to describe."
  (interactive (list (or (kubernetes-utils-maybe-pod-name-at-point) (kubernetes-pods--read-name (kubernetes-state)))))
  (let ((buf (get-buffer-create kubernetes-pod-buffer-name))
        (marker (make-marker)))
    (with-current-buffer buf
      (kubernetes-display-thing-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (set-marker marker (point))
        (insert (propertize "Loading..." 'face 'kubernetes-dimmed))))
    (let* ((populate-buffer (lambda (s)
                              (with-current-buffer (marker-buffer marker)
                                (setq-local tab-width 8)
                                (let ((inhibit-read-only t)
                                      (inhibit-redisplay t))
                                  (erase-buffer)
                                  (insert "---\n")
                                  (insert s)
                                  (untabify (point-min) (point-max))
                                  (goto-char (point-min))))))
           (proc (kubernetes-kubectl-describe-pod (kubernetes-state)
                                                  pod-name
                                                  populate-buffer)))
      (with-current-buffer buf
        (add-hook 'kill-buffer-hook (lambda () (kubernetes-process-kill-quietly proc)) nil t)))

    (select-window (display-buffer buf))
    buf))

;;;###autoload
(defun kubernetes-exec-into (pod-name args exec-command state)
  "Open a terminal for execting into a pod.

POD-NAME is the name of the pod to exec into.

ARGS are additional args to pass to kubectl.

EXEC-COMMAND is the command to run in the container.

STATE is the current application state.

Should be invoked via command `kubernetes-exec'."
  (interactive (let* ((state (kubernetes-state))
                      (pod-name (or (kubernetes-utils-maybe-pod-name-at-point) (kubernetes-pods--read-name state)))
                      (command
                       (let ((cmd (string-trim (read-string (format "Command (default: %s): " kubernetes-default-exec-command)
                                                            nil 'kubernetes-exec-history))))
                         (if (string-empty-p cmd) kubernetes-default-exec-command cmd))))
                 (list pod-name (transient-args 'kubernetes-exec) command state)))

  (let* ((command-args (append (list "exec") (kubernetes-kubectl--flags-from-state (kubernetes-state))
                               args
                               (when-let (ns (kubernetes-state--get state 'current-namespace))
                                 (list (format "--namespace=%s" ns)))
                               (list pod-name "--" exec-command)))

         (interactive-tty (member "--tty" args))
         (buf
          (if interactive-tty
              (kubernetes-utils-term-buffer-start kubernetes-exec-buffer-name
                                                  kubernetes-kubectl-executable
                                                  command-args)
            (kubernetes-utils-process-buffer-start kubernetes-exec-buffer-name
                                                   #'kubernetes-mode
                                                   kubernetes-kubectl-executable
                                                   command-args))))

    (when (and interactive-tty kubernetes-clean-up-interactive-exec-buffers)
      (set-process-sentinel (get-buffer-process buf) #'kubernetes-process-kill-quietly))

    (select-window (display-buffer buf))))


;;;###autoload
(defun kubernetes-exec-using-vterm (pod-name args exec-command state)
  "Open a vterm terminal for exec into a pod.

POD-NAME is the name of the pod to exec into.

ARGS are additional args to pass to kubectl.

EXEC-COMMAND is the command to run in the container.

STATE is the current application state.

Should be invoked via command `kubernetes-exec'."
  (interactive (let* ((state (kubernetes-state))
                      (pod-name (or (kubernetes-utils-maybe-pod-name-at-point) (kubernetes-pods--read-name state)))
                      (command
                       (let ((cmd (string-trim (read-string (format "Command (default: %s): " kubernetes-default-exec-command)
                                                            nil 'kubernetes-exec-history))))
                         (if (string-empty-p cmd) kubernetes-default-exec-command cmd))))
                 (list pod-name (transient-args 'kubernetes-exec) command state)))

  (unless (require 'vterm nil 'noerror)
    (error "This action requires the vterm package."))

  (let* ((command-args (append (list "exec") (kubernetes-kubectl--flags-from-state (kubernetes-state))
                               args
                               (when-let (ns (kubernetes-state--get state 'current-namespace))
                                 (list (format "--namespace=%s" ns)))
                               (list pod-name "--" exec-command)))

         (interactive-tty (member "--tty" args)))
    (if interactive-tty
        (kubernetes-utils-vterm-start kubernetes-exec-vterm-buffer-name
                                      kubernetes-kubectl-executable
                                      command-args)
      (error "VTerm is supported only for interactive session"))))

;; View management

(defun kubernetes-commands-display-buffer-fullframe (buffer)
  (let ((display-fn
         (lambda (buffer alist)
           (when-let (window (or (display-buffer-reuse-window buffer alist)
                                 (display-buffer-same-window buffer alist)
                                 (display-buffer-pop-up-window buffer alist)
                                 (display-buffer-use-some-window buffer alist)))
             (delete-other-windows window)
             window))))
    (display-buffer buffer (list display-fn))))

(defun kubernetes-commands-display-buffer (buffer)
  (let ((window (funcall kubernetes-commands-display-buffer-function buffer)))
    (when kubernetes-commands-display-buffer-select
      (select-frame-set-input-focus
       (window-frame (select-window window))))))


;; Config management

;;;###autoload
(defun kubernetes-set-namespace (ns state)
  "Set the namespace to query to NS.

Overrides the namespace settings for the current context.

STATE is the current application state."
  (interactive
   (let ((state (kubernetes-state)))
     (list (completing-read "Use namespace: " (kubernetes--namespace-names state) nil t)
           state)))
  (kubernetes-process-kill-polling-processes)
  (kubernetes-state-clear)
  (goto-char (point-min))

  ;; State for the context and view should be preserved.
  (kubernetes-state-update-config (kubernetes-state--get state 'config))
  (kubernetes-state-update-current-namespace ns)
  (kubernetes-kubectl-config-set-current-namespace (kubernetes-state)
                                                   (lambda (_)
                                                     (kubernetes--info "Updated current context `%s' namespace to `%s.'"
                                                              (alist-get 'name (kubernetes-state-current-context state))
                                                              ns))
                                                   (lambda (buf)
                                                     (let ((s (with-current-buffer buf (buffer-string))))
                                                       (kubernetes--error "Unable to set namespace `%s' for current context `%s'."
                                                                ns
                                                                (alist-get 'name (kubernetes-state-current-context state)))
                                                       (kubernetes--message s))))

  (kubernetes-state-update-overview-sections (kubernetes-state-overview-sections state))

  (kubernetes-state-trigger-redraw))

(defun kubernetes--namespace-names (state)
  (-let* ((config (or (kubernetes-state--get state 'namespaces) (kubernetes-kubectl-await-on-async state (-partial #'kubernetes-kubectl-get "namespaces"))))
          ((&alist 'items items) config))
    (-map (-lambda ((&alist 'metadata (&alist 'name name))) name) items)))

(defun kubernetes--edit-resource (kind name)
  (kubernetes-kubectl-edit-resource (kubernetes-state)
                                    kind
                                    name
                                    (lambda (buf)
                                      (let ((s (with-current-buffer buf (buffer-string))))
                                        (message "Edited resource %s of kind %s" name kind)
                                        (message s)))
                                    (lambda (buf)
                                      (let ((s (with-current-buffer buf (buffer-string))))
                                        (message "Editing resource %s of kind %s failed" name kind)
                                        (message s)))))

;;;###autoload
(defun kubernetes-edit-resource-dwim (thing)
  "Edit the resource at point.

THING must be a valid target for `kubectl edit'."
  (interactive (list (kubernetes--describable-thing-at-pt)))
  (pcase thing
    (`(:configmap-name ,name)
     (kubernetes--edit-resource "configmap" name))
    (`(:deployment-name ,name)
     (kubernetes--edit-resource "deployment" name))
    (`(:ingress-name ,name)
     (kubernetes--edit-resource "ingress" name))
    (`(:job-name ,name)
     (kubernetes--edit-resource "job" name))
    (`(:node-name ,name)
     (kubernetes--edit-resource "node" name))
    (`(:persistentvolumeclaim-name ,name)
     (kubernetes--edit-resource "persistentvolumeclaim" name))
    (`(:pod-name ,name)
     (kubernetes--edit-resource "pod" name))
    (`(:secret-name ,name)
     (kubernetes--edit-resource "secret" name))
    (`(:service-name ,name)
     (kubernetes--edit-resource "service" name))
    (`(:statefulset-name ,name)
     (kubernetes--edit-resource "statefulset" name))
    (_
     (user-error "Nothing at point to edit"))))

(provide 'kubernetes-commands)

;;; kubernetes-commands.el ends here
