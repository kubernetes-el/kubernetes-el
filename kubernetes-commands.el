;;; kubernetes-commands.el --- Interactive commands for Kubernetes modes.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'kubernetes-configmaps)
(require 'kubernetes-contexts)
(require 'kubernetes-namespaces)
(require 'kubernetes-pods)
(require 'kubernetes-popups)
(require 'kubernetes-secrets)
(require 'kubernetes-services)
(require 'kubernetes-state)


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
    (`(:secret-name ,name)
     (kubernetes-state-mark-secret name))
    (`(:service-name ,name)
     (kubernetes-state-mark-service name))
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
    (`(:pod-name ,pod-name)
     (kubernetes-state-unmark-pod pod-name))
    (`(:secret-name ,secret-name)
     (kubernetes-state-unmark-secret secret-name))
    (`(:configmap-name ,configmap-name)
     (kubernetes-state-unmark-configmap configmap-name)))
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
    (let ((n (length (kubernetes-state-marked-pods state))))
      (when (and (not (zerop n))
                 (y-or-n-p (format "Delete %s pod%s? " n (if (equal 1 n) "" "s"))))
        (kubernetes-pods-delete-marked state)))

    (let ((n (length (kubernetes-state-marked-configmaps state))))
      (when (and (not (zerop n))
                 (y-or-n-p (format "Delete %s configmap%s? " n (if (equal 1 n) "" "s"))))
        (kubernetes-configmaps-delete-marked state)))

    (let ((n (length (kubernetes-state-marked-secrets state))))
      (when (and (not (zerop n))
                 (y-or-n-p (format "Delete %s secret%s? " n (if (equal 1 n) "" "s"))))
        (kubernetes-secrets-delete-marked state)))

    (let ((n (length (kubernetes-state-marked-services state))))
      (when (and (not (zerop n))
                 (y-or-n-p (format "Delete %s service%s? " n (if (equal 1 n) "" "s"))))
        (kubernetes-services-delete-marked state))))

  (kubernetes-unmark-all))


;; Misc commands

;;;###autoload
(defun kubernetes-copy-thing-at-point (point)
  "Perform a context-sensitive copy action.

Inspecs the `kubernetes-copy' text property at POINT to determine
what to copy."
  (interactive "d")
  (when-let (s (get-text-property point 'kubernetes-copy))
    (kill-new s)
    (message "Copied: %s" s)))

;;;###autoload
(defun kubernetes-refresh (&optional verbose)
  "Force Kubernetes buffers to redraw.

With optional argument VERBOSE, log status changes."
  (interactive "p")
  (kubernetes--poll verbose)
  (kubernetes-state-trigger-redraw))

(defun kubernetes--poll (&optional verbose)
  (kubernetes-configmaps-refresh verbose)
  (kubernetes-contexts-refresh verbose)
  (kubernetes-namespaces-refresh verbose)
  (kubernetes-pods-refresh verbose)
  (kubernetes-secrets-refresh verbose)
  (kubernetes-services-refresh verbose))

;;;###autoload
(defun kubernetes-navigate (point state)
  "Perform a context-sensitive navigation action.

STATE is the current application state.

Inspecs the `kubernetes-nav' text property at POINT to determine
how to navigate.  If that property is not found, no action is
taken."
  (interactive (list (point) (kubernetes-state)))
  (pcase (get-text-property point 'kubernetes-nav)
    (:display-config
     (kubernetes-display-config (alist-get 'config state)))
    (`(:configmap-name ,configmap-name)
     (kubernetes-display-configmap configmap-name state))
    (`(:service-name ,service-name)
     (kubernetes-display-service service-name state))
    (`(:secret-name ,secret-name)
     (kubernetes-display-secret secret-name state))
    (`(:pod-name ,pod-name)
     (kubernetes-display-pod pod-name state))))

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
  (interactive (list (or (kubernetes-utils-maybe-pod-name-at-point) (kubernetes-utils-read-pod-name (kubernetes-state)))))
  (let ((buf (get-buffer-create kubernetes-pod-buffer-name))
        (marker (make-marker)))
    (with-current-buffer buf
      (kubernetes-display-thing-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (set-marker marker (point))
        (insert (propertize "Loading..." 'face 'magit-dimmed))))
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
           (proc (kubernetes-kubectl-describe-pod kubernetes-default-props
                                                  (kubernetes-state)
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

Should be invoked via command `kubernetes-logs-popup'."
  (interactive (let* ((state (kubernetes-state))
                      (pod-name (or (kubernetes-utils-maybe-pod-name-at-point) (kubernetes-utils-read-pod-name state)))
                      (command
                       (let ((cmd (string-trim (read-string (format "Command (default: %s): " kubernetes-default-exec-command)
                                                            nil 'kubernetes-exec-history))))
                         (if (string-empty-p cmd) kubernetes-default-exec-command cmd))))
                 (list pod-name (kubernetes-exec-arguments) command state)))

  (let* ((command-args (append (list "exec")
                               args
                               (when-let (ns (kubernetes-state-current-namespace state))
                                 (list (format "--namespace=%s" ns)))
                               (list pod-name exec-command)))

         (interactive-tty (member "-t" args))
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


;; View management

(defun kubernetes-utils-display-buffer-fullframe (buffer)
  (let ((display-fn
         (lambda (buffer alist)
           (when-let (window (or (display-buffer-reuse-window buffer alist)
                                 (display-buffer-same-window buffer alist)
                                 (display-buffer-pop-up-window buffer alist)
                                 (display-buffer-use-some-window buffer alist)))
             (delete-other-windows window)
             window))))
    (display-buffer buffer (list display-fn))))

(defun kubernetes-utils-display-buffer (buffer)
  (let ((window (funcall kubernetes-utils-display-buffer-function buffer)))
    (when kubernetes-utils-display-buffer-select
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
  ;; The context is safe to preserve, but everything else should be reset.
  (let ((config (kubernetes-state-config state)))
    (kubernetes-process-kill-polling-processes)
    (kubernetes-state-clear)
    (goto-char (point-min))
    (kubernetes-state-update-config config)
    (kubernetes-state-update-current-namespace ns)
    (kubernetes-state-trigger-redraw)))

(defun kubernetes--namespace-names (state)
  (-let* ((config (or (kubernetes-state-namespaces state) (kubernetes-kubectl-await-on-async kubernetes-default-props state #'kubernetes-kubectl-get-namespaces)))
          ((&alist 'items items) config))
    (-map (-lambda ((&alist 'metadata (&alist 'name name))) name) items)))

(defun kubernetes-use-context (context)
  "Switch Kubernetes context refresh the pods buffer.

CONTEXT is the name of a context as a string."
  (interactive (list (completing-read "Context: " (kubernetes--context-names (kubernetes-state)) nil t)))
  (kubernetes-process-kill-polling-processes)
  (kubernetes-state-clear)
  (kubernetes-state-trigger-redraw)
  (goto-char (point-min))
  (kubernetes-kubectl-config-use-context kubernetes-default-props
                                         (kubernetes-state)
                                         context
                                         (lambda (_)
                                           (kubernetes-state-trigger-redraw))))

(defun kubernetes--context-names (state)
  (-let* ((config (or (kubernetes-state-config state) (kubernetes-kubectl-await-on-async kubernetes-default-props state #'kubernetes-kubectl-config-view)))
          ((&alist 'contexts contexts) config))
    (--map (alist-get 'name it) contexts)))

(provide 'kubernetes-commands)

;;; kubernetes-commands.el ends here
