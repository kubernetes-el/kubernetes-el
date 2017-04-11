;;; kubernetes.el --- Emacs porcelain for Kubernetes.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;; Version: 0.2.3

;; Package-Requires: ((emacs "25.1") (dash "2.12.0") (magit "2.8.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'dash)
(require 'magit-popup)
(require 'subr-x)

(require 'kubernetes-ast)
(require 'kubernetes-configmaps)
(require 'kubernetes-contexts)
(require 'kubernetes-errors)
(require 'kubernetes-kubectl)
(require 'kubernetes-pods)
(require 'kubernetes-secrets)
(require 'kubernetes-services)
(require 'kubernetes-state)
(require 'kubernetes-utils)
(require 'kubernetes-vars)

(autoload 'json-pretty-print-buffer "json")


;; Display pod view rendering routines.

(defun kubernetes--display-pods-initialize-buffer ()
  "Called the first time the pods buffer is opened to set up the buffer."
  (let ((buf (get-buffer-create kubernetes-display-pods-buffer-name)))
    (with-current-buffer buf
      (kubernetes-display-pods-mode)

      ;; Render buffer.
      (kubernetes--redraw-pods-buffer t)
      (goto-char (point-min))

      (kubernetes-state-initialize-timers)
      (add-hook 'kill-buffer-hook (kubernetes--make-cleanup-fn buf) nil t))
    buf))

(defun kubernetes--redraw-pods-buffer (&optional force)
  "Redraws the main buffer using the current state.

FORCE ensures it happens."
  (when-let (buf (get-buffer kubernetes-display-pods-buffer-name))
    (with-current-buffer buf
      (when (or force
                ;; HACK: Only redraw the buffer if it is in the selected window.
                ;;
                ;; The cursor moves unpredictably in a redraw, which ruins the current
                ;; position in the buffer if a popup window is open.
                (equal (window-buffer) buf))

        (let ((pos (point))
              (inhibit-read-only t)
              (inhibit-redisplay t)
              (state (kubernetes--state)))

          (-when-let ((&alist 'pods (&alist 'items pods)) state)
            (kubernetes-state-update-marked-pods (append pods nil)))

          (erase-buffer)
          (kubernetes-ast-eval `(section (root nil)
                               ,(kubernetes-errors-render state)
                               ,(kubernetes-contexts-render state)
                               ,(kubernetes-pods-render state)))
          (goto-char pos)))

      ;; Force the section at point to highlight.
      (magit-section-update-highlight))))


;; Display configmap view rendering routines.

(defun kubernetes--display-configmaps-initialize-buffer ()
  "Called the first time the configmaps buffer is opened to set up the buffer."
  (let ((buf (get-buffer-create kubernetes-display-configmaps-buffer-name)))
    (with-current-buffer buf
      (kubernetes-display-configmaps-mode)

      ;; Render buffer.
      (kubernetes--redraw-configmaps-buffer t)
      (goto-char (point-min))

      (kubernetes-state-initialize-timers)
      (add-hook 'kill-buffer-hook (kubernetes--make-cleanup-fn buf) nil t))
    buf))

(defun kubernetes--redraw-configmaps-buffer (&optional force)
  "Redraws the main buffer using the current state.

FORCE ensures it happens."
  (when-let (buf (get-buffer kubernetes-display-configmaps-buffer-name))
    (with-current-buffer buf
      (when (or force
                ;; HACK: Only redraw the buffer if it is in the selected window.
                ;;
                ;; The cursor moves unpredictably in a redraw, which ruins the current
                ;; position in the buffer if a popup window is open.
                (equal (window-buffer) buf))

        (let ((pos (point))
              (inhibit-read-only t)
              (inhibit-redisplay t)
              (state (kubernetes--state)))

          (erase-buffer)
          (kubernetes-ast-eval `(section (root nil)
                               ,(kubernetes-errors-render state)
                               ,(kubernetes-contexts-render state)
                               ,(kubernetes-configmaps-render state)))
          (goto-char pos)))

      ;; Force the section at point to highlight.
      (magit-section-update-highlight))))


;; Display secret view rendering routines.

(defun kubernetes--display-secrets-initialize-buffer ()
  "Called the first time the secrets buffer is opened to set up the buffer."
  (let ((buf (get-buffer-create kubernetes-display-secrets-buffer-name)))
    (with-current-buffer buf
      (kubernetes-display-secrets-mode)

      ;; Render buffer.
      (kubernetes--redraw-secrets-buffer t)
      (goto-char (point-min))

      (kubernetes-state-initialize-timers)
      (add-hook 'kill-buffer-hook (kubernetes--make-cleanup-fn buf) nil t))
    buf))

(defun kubernetes--redraw-secrets-buffer (&optional force)
  "Redraws the main buffer using the current state.

FORCE ensures it happens."
  (when-let (buf (get-buffer kubernetes-display-secrets-buffer-name))
    (with-current-buffer buf
      (when (or force
                ;; HACK: Only redraw the buffer if it is in the selected window.
                ;;
                ;; The cursor moves unpredictably in a redraw, which ruins the current
                ;; position in the buffer if a popup window is open.
                (equal (window-buffer) buf))

        (let ((pos (point))
              (inhibit-read-only t)
              (inhibit-redisplay t)
              (state (kubernetes--state)))

          (erase-buffer)
          (kubernetes-ast-eval `(section (root nil)
                               ,(kubernetes-errors-render state)
                               ,(kubernetes-contexts-render state)
                               ,(kubernetes-secrets-render state)))
          (goto-char pos)))

      ;; Force the section at point to highlight.
      (magit-section-update-highlight))))


;; Displaying config.

(defun kubernetes-display-config-refresh (config)
  (let ((buf (get-buffer-create kubernetes-display-config-buffer-name)))
    (with-current-buffer buf
      (kubernetes-display-thing-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (kubernetes--json-to-yaml config))))
    buf))

;;;###autoload
(defun kubernetes-display-config (config)
  "Display information for CONFIG in a new window."
  (interactive (list (kubernetes-kubectl-await-on-async #'kubernetes-kubectl-config-view)))
  (with-current-buffer (kubernetes-display-config-refresh config)
    (goto-char (point-min))
    (select-window (display-buffer (current-buffer)))))


;; Displaying configmaps.

(defun kubernetes-display-configmap-refresh (configmap-name)
  (if-let (configmap (kubernetes-state-lookup-configmap configmap-name))
      (let ((buf (get-buffer-create kubernetes-display-configmap-buffer-name)))
        (with-current-buffer buf
          (kubernetes-display-thing-mode)
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (kubernetes--json-to-yaml configmap))))
        buf)
    (error "Unknown configmap: %s" configmap-name)))

;;;###autoload
(defun kubernetes-display-configmap (configmap-name)
  "Display information for a configmap in a new window.

CONFIGMAP-NAME is the name of the configmap to display."
  (interactive (list (kubernetes--read-configmap-name)))
  (with-current-buffer (kubernetes-display-configmap-refresh configmap-name)
    (goto-char (point-min))
    (select-window (display-buffer (current-buffer)))))


;; Displaying secrets

(defun kubernetes-display-secret-refresh (secret-name)
  (if-let (secret (kubernetes-state-lookup-secret secret-name))
      (let ((buf (get-buffer-create kubernetes-display-secret-buffer-name)))
        (with-current-buffer buf
          (kubernetes-display-thing-mode)
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (kubernetes--json-to-yaml secret))))
        buf)
    (error "Unknown secret: %s" secret-name)))

;;;###autoload
(defun kubernetes-display-secret (secret-name)
  "Display information for a secret in a new window.

SECRET-NAME is the name of the secret to display."
  (interactive (list (kubernetes--read-secret-name)))
  (with-current-buffer (kubernetes-display-secret-refresh secret-name)
    (goto-char (point-min))
    (select-window (display-buffer (current-buffer)))))


;; Displaying services

(defun kubernetes-display-service-refresh (service-name)
  (if-let (service (kubernetes-state-lookup-service service-name))
      (let ((buf (get-buffer-create kubernetes-display-service-buffer-name)))
        (with-current-buffer buf
          (kubernetes-display-thing-mode)
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (kubernetes--json-to-yaml service))))
        buf)
    (error "Unknown service: %s" service-name)))

;;;###autoload
(defun kubernetes-display-service (service-name)
  "Display information for a service in a new window.

SERVICE-NAME is the name of the service to display."
  (interactive (list (kubernetes--read-service-name)))
  (with-current-buffer (kubernetes-display-service-refresh service-name)
    (goto-char (point-min))
    (select-window (display-buffer (current-buffer)))))


;; Displaying pods.

(defun kubernetes-display-pod-refresh (pod-name)
  (if-let (pod (kubernetes-state-lookup-pod pod-name))
      (let ((buf (get-buffer-create kubernetes-pod-buffer-name)))
        (with-current-buffer buf
          (kubernetes-display-thing-mode)
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (kubernetes--json-to-yaml pod))))
        buf)
    (error "Unknown pod: %s" pod-name)))

;;;###autoload
(defun kubernetes-display-pod (pod-name)
  "Display information for a pod in a new window.

POD-NAME is the name of the pod to display."
  (interactive (list (kubernetes--read-pod-name)))
  (with-current-buffer (kubernetes-display-pod-refresh pod-name)
    (goto-char (point-min))
    (select-window (display-buffer (current-buffer)))))


;; Marking pods for deletion

(defun kubernetes-mark-for-delete (point)
  "Mark the thing at POINT for deletion, then advance to the next line."
  (interactive "d")
  (pcase (get-text-property point 'kubernetes-nav)
    (`(:pod-name ,pod-name)
     (unless (member pod-name kubernetes-state--pods-pending-deletion)
       (add-to-list 'kubernetes-state--marked-pod-names pod-name)))
    (`(:configmap-name ,configmap-name)
     (unless (member configmap-name kubernetes-state--configmaps-pending-deletion)
       (add-to-list 'kubernetes-state--marked-configmap-names configmap-name)))
    (`(:secret-name ,secret-name)
     (unless (member secret-name kubernetes-state--secrets-pending-deletion)
       (add-to-list 'kubernetes-state--marked-secret-names secret-name)))
    (_
     (user-error "Nothing here can be marked")))

  (let ((inhibit-read-only t))
    (kubernetes-ast-put-delete-mark-on-line-at-pt point))
  (magit-section-forward))

(defun kubernetes-unmark (point)
  "Unmark the thing at POINT, then advance to the next line."
  (interactive "d")
  (pcase (get-text-property point 'kubernetes-nav)
    (`(:pod-name ,pod-name)
     (setq kubernetes-state--marked-pod-names (delete pod-name kubernetes-state--marked-pod-names)))
    (`(:secret-name ,secret-name)
     (setq kubernetes-state--marked-secret-names (delete secret-name kubernetes-state--marked-secret-names)))
    (`(:configmap-name ,configmap-name)
     (setq kubernetes-state--marked-configmap-names (delete configmap-name kubernetes-state--marked-configmap-names))))
  (kubernetes--redraw-buffers)
  (goto-char point)
  (magit-section-forward))

(defun kubernetes-unmark-all ()
  "Unmark everything in the buffer."
  (interactive)
  (setq kubernetes-state--marked-pod-names nil)
  (setq kubernetes-state--marked-configmap-names nil)
  (setq kubernetes-state--marked-secret-names nil)
  (let ((pt (point)))
    (kubernetes--redraw-buffers)
    (goto-char pt)))

(defun kubernetes-execute-marks ()
  "Action all marked items in the buffer."
  (interactive)
  (unless (or kubernetes-state--marked-pod-names
              kubernetes-state--marked-configmap-names
              kubernetes-state--marked-secret-names)
    (user-error "Nothing is marked"))

  (let ((n (length kubernetes-state--marked-pod-names)))
    (when (and (not (zerop n))
               (y-or-n-p (format "Delete %s pod%s? " n (if (equal 1 n) "" "s"))))
      (kubernetes--delete-marked-pods)
      (kubernetes-unmark-all)))

  (let ((n (length kubernetes-state--marked-configmap-names)))
    (when (and (not (zerop n))
               (y-or-n-p (format "Delete %s configmap%s? " n (if (equal 1 n) "" "s"))))
      (kubernetes--delete-marked-configmaps)
      (kubernetes-unmark-all)))

  (let ((n (length kubernetes-state--marked-secret-names)))
    (when (and (not (zerop n))
               (y-or-n-p (format "Delete %s secret%s? " n (if (equal 1 n) "" "s"))))
      (kubernetes--delete-marked-secrets)
      (kubernetes-unmark-all))))

(defun kubernetes--delete-marked-pods ()
  (let ((n (length kubernetes-state--marked-pod-names)))
    (message "Deleting %s pod%s..." n (if (equal 1 n) "" "s"))
    (dolist (pod kubernetes-state--marked-pod-names)
      (add-to-list 'kubernetes-state--pods-pending-deletion pod)

      (kubernetes-kubectl-delete-pod pod
                           (lambda (_)
                             (message "Deleting pod %s succeeded." pod)
                             (kubernetes-refresh))
                           (lambda (_)
                             (message "Deleting pod %s failed" pod)
                             (setq kubernetes-state--pods-pending-deletion (delete pod kubernetes-state--pods-pending-deletion)))))))

(defun kubernetes--delete-marked-configmaps ()
  (let ((n (length kubernetes-state--marked-configmap-names)))
    (message "Deleting %s configmap%s..." n (if (equal 1 n) "" "s"))
    (dolist (configmap kubernetes-state--marked-configmap-names)
      (add-to-list 'kubernetes-state--configmaps-pending-deletion configmap)

      (kubernetes-kubectl-delete-configmap configmap
                                 (lambda (_)
                                   (message "Deleting configmap %s succeeded." configmap)
                                   (kubernetes-refresh))
                                 (lambda (_)
                                   (message "Deleting configmap %s failed" configmap)
                                   (setq kubernetes-state--configmaps-pending-deletion (delete configmap kubernetes-state--configmaps-pending-deletion)))))))

(defun kubernetes--delete-marked-secrets ()
  (let ((n (length kubernetes-state--marked-secret-names)))
    (message "Deleting %s secret%s..." n (if (equal 1 n) "" "s"))
    (dolist (secret kubernetes-state--marked-secret-names)
      (add-to-list 'kubernetes-state--secrets-pending-deletion secret)

      (kubernetes-kubectl-delete-secret secret
                              (lambda (_)
                                (message "Deleting secret %s succeeded." secret)
                                (kubernetes-refresh))
                              (lambda (_)
                                (message "Deleting secret %s failed" secret)
                                (setq kubernetes-state--secrets-pending-deletion (delete secret kubernetes-state--secrets-pending-deletion)))))))


;;; Misc interactive commands

(defun kubernetes-navigate (point)
  "Perform a context-sensitive navigation action.

Inspecs the `kubernetes-nav' text property at POINT to determine
how to navigate.  If that property is not found, no action is
taken."
  (interactive "d")
  (pcase (get-text-property point 'kubernetes-nav)
    (:display-config
     (kubernetes-display-config (alist-get 'config (kubernetes--state))))
    (`(:configmap-name ,configmap-name)
     (kubernetes-display-configmap configmap-name))
    (`(:service-name ,service-name)
     (kubernetes-display-service service-name))
    (`(:secret-name ,secret-name)
     (kubernetes-display-secret secret-name))
    (`(:pod-name ,pod-name)
     (kubernetes-display-pod pod-name))))

(defun kubernetes-copy-thing-at-point (point)
  "Perform a context-sensitive copy action.

Inspecs the `kubernetes-copy' text property at POINT to determine
what to copy."
  (interactive "d")
  (when-let (s (get-text-property point 'kubernetes-copy))
    (kill-new s)
    (message "Copied: %s" s)))

(defun kubernetes--redraw-buffers (&optional force)
  (kubernetes--redraw-pods-buffer force)
  (kubernetes--redraw-configmaps-buffer force)
  (kubernetes--redraw-secrets-buffer force)
  (kubernetes--redraw-overview-buffer force))

(defun kubernetes-refresh (&optional interactive)
  "Trigger a manual refresh Kubernetes pods buffers.

Requests the data needed to build the buffers.

With optional argument INTERACTIVE, redraw the buffer and log
additional information of state changes."
  (interactive (list t))
  ;; Make sure not to trigger a refresh if the buffer closes.
  (when (or (get-buffer kubernetes-display-configmaps-buffer-name)
            (get-buffer kubernetes-display-secrets-buffer-name)
            (get-buffer kubernetes-display-pods-buffer-name)
            (get-buffer kubernetes-overview-buffer-name))
    (when interactive
      (kubernetes--redraw-buffers)
      (message "Refreshing..."))

    (unless (kubernetes-state-poll-namespaces-process-live-p)
      (kubernetes-state-set-poll-namespaces-process
       (kubernetes-kubectl-get-namespaces
        (lambda (config)
          (setq kubernetes-state--get-namespaces-response config)
          (when interactive
            (message "Updated namespaces.")))
        (lambda ()
          (kubernetes-state-release-poll-namespaces-process)))))

    (unless (kubernetes-state-poll-context-process-live-p)
      (kubernetes-state-set-poll-context-process
       (kubernetes-kubectl-config-view
        (lambda (config)
          (setq kubernetes-state--view-config-response config)
          (when interactive
            (message "Updated contexts.")))
        (lambda ()
          (kubernetes-state-release-poll-context-process)))))

    (unless (kubernetes-state-poll-configmaps-process-live-p)
      (kubernetes-state-set-poll-configmaps-process
       (kubernetes-kubectl-get-configmaps
        (lambda (response)
          (setq kubernetes-state--get-configmaps-response response)
          (when interactive
            (message "Updated configmaps.")))
        (lambda ()
          (kubernetes-state-release-poll-configmaps-process)))))

    (unless (kubernetes-state-poll-services-process-live-p)
      (kubernetes-state-set-poll-services-process
       (kubernetes-kubectl-get-services
        (lambda (response)
          (setq kubernetes-state--get-services-response response)
          (when interactive
            (message "Updated services.")))
        (lambda ()
          (kubernetes-state-release-poll-services-process)))))

    (unless (kubernetes-state-poll-secrets-process-live-p)
      (kubernetes-state-set-poll-secrets-process
       (kubernetes-kubectl-get-secrets
        (lambda (response)
          (setq kubernetes-state--get-secrets-response response)
          (when interactive
            (message "Updated secrets.")))
        (lambda ()
          (kubernetes-state-release-poll-secrets-process)))))

    (unless (kubernetes-state-poll-pods-process-live-p)
      (kubernetes-state-set-poll-pods-process
       (kubernetes-kubectl-get-pods
        (lambda (response)
          (setq kubernetes-state--get-pods-response response)
          (when interactive
            (message "Updated pods.")))
        (lambda ()
          (kubernetes-state-release-poll-pods-process)))))))


;; Logs

(defun kubernetes--log-line-buffer-for-string (s)
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

(defun kubernetes-logs-inspect-line (pos)
  "Show detail for the log line at POS."
  (interactive "d")
  (display-buffer (kubernetes--log-line-buffer-for-string
                   (save-excursion
                     (goto-char pos)
                     (buffer-substring (line-beginning-position) (line-end-position))))))

(defun kubernetes-logs-previous-line ()
  "Move backward and inspect the line at point."
  (interactive)
  (with-current-buffer kubernetes-logs-buffer-name
    (forward-line -1)
    (when (get-buffer kubernetes-log-line-buffer-name)
      (kubernetes-logs-inspect-line (point)))))

(defun kubernetes-logs-forward-line ()
  "Move forward and inspect the line at point."
  (interactive)
  (with-current-buffer kubernetes-logs-buffer-name
    (forward-line 1)
    (when (get-buffer kubernetes-log-line-buffer-name)
      (kubernetes-logs-inspect-line (point)))))


;; Popups

(defgroup kubernetes nil
  "Emacs porcelain for Kubernetes."
  :group 'tools
  :prefix "kubernetes-")


(magit-define-popup kubernetes-logs-popup
  "Popup console for logging commands for POD."
  :group 'kubernetes

  :options
  '("Options for customizing logging behaviour"
    (?t "Number of lines to display" "--tail=" read-number "-1")
    "Time controls"
    (?s "Since relative time" "--since=" kubernetes--read-time-value)
    (?d "Since absolute datetime" "--since-time=" kubernetes--read-iso-datetime))

  :switches
  '((?p "Print logs for previous instances of the container in this pod" "-p"))

  :actions
  '((?l "Logs" kubernetes-logs-fetch-all)
    (?f "Logs (stream and follow)" kubernetes-logs-follow))

  :max-action-columns 2

  :default-action 'kubernetes-logs)

(defun kubernetes-logs-follow (pod-name args)
  "Open a streaming logs buffer for a pod.

POD-NAME is the name of the pod to log.

ARGS are additional args to pass to kubectl."
  (interactive (list (or (kubernetes--maybe-pod-name-at-point) (kubernetes--read-pod-name))
                     (kubernetes-logs-arguments)))
  (kubernetes-logs-fetch-all pod-name (cons "-f" args)))

(defun kubernetes-logs-fetch-all (pod-name args)
  "Open a streaming logs buffer for POD.

POD-NAME is the name of the pod to log.

ARGS are additional args to pass to kubectl."
  (interactive (list (or (kubernetes--maybe-pod-name-at-point) (kubernetes--read-pod-name))
                     (kubernetes-logs-arguments)))
  (let ((args (append (list "logs") args (list pod-name)
                      (when kubernetes-state--current-namespace
                        (list (format "--namespace=%s" kubernetes-state--current-namespace))))))
    (with-current-buffer (kubernetes-process-buffer-start kubernetes-logs-buffer-name #'kubernetes-logs-mode kubernetes-kubectl-executable args)
      (select-window (display-buffer (current-buffer))))))


(magit-define-popup kubernetes-describe-popup
  "Popup console for describe commands."
  :group 'kubernetes

  :actions
  '((?d "Dwim" kubernetes-describe-dwim)
    (?p "Pod" kubernetes-describe-pod))

  :default-action 'kubernetes-logs)

(defun kubernetes--describable-thing-at-pt ()
  (save-excursion
    (back-to-indentation)
    (get-text-property (point) 'kubernetes-nav)))

(defun kubernetes-describe-dwim (thing)
  "Describe the thing at point.

THING must be a valid target for `kubectl describe'."
  (interactive (list (kubernetes--describable-thing-at-pt)))
  (pcase thing
    (`(:pod-name ,pod-name)
     (kubernetes-describe-pod pod-name))
    (_
     (user-error "Nothing at point to describe"))))

(defun kubernetes-describe-pod (pod-name)
  "Display a buffer for describing a pod.

POD-NAME is the name of the pod to describe."
  (interactive (list (or (kubernetes--maybe-pod-name-at-point) (kubernetes--read-pod-name))))
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
           (proc (kubernetes-kubectl-describe-pod pod-name populate-buffer)))
      (with-current-buffer buf
        (add-hook 'kill-buffer-hook (lambda () (kubernetes-process-kill-quietly proc)) nil t)))

    (select-window (display-buffer buf))
    buf))


(magit-define-popup kubernetes-exec-popup
  "Popup console for exec commands for POD."
  :group 'kubernetes

  :default-arguments '("-i" "-t")

  :switches
  '((?i "Pass stdin to container" "-i" t)
    (?t "Stdin is a TTY" "-t" t))

  :actions
  '((?e "Exec" kubernetes-exec-into))

  :default-action 'kubernetes-exec-into)

(defun kubernetes-exec-into (pod-name args exec-command)
  "Open a terminal for execting into a pod.

POD-NAME is the name of the pod to exec into.

ARGS are additional args to pass to kubectl.

EXEC-COMMAND is the command to run in the container.

Should be invoked via command `kubernetes-logs-popup'."
  (interactive (list (or (kubernetes--maybe-pod-name-at-point) (kubernetes--read-pod-name))
                     (kubernetes-exec-arguments)
                     (let ((cmd (string-trim (read-string (format "Command (default: %s): " kubernetes-default-exec-command)
                                                          nil 'kubernetes-exec-history))))
                       (if (string-empty-p cmd) kubernetes-default-exec-command cmd))))

  (let* ((command-args (append (list "exec")
                               args
                               (when kubernetes-state--current-namespace
                                 (list (format "--namespace=%s" kubernetes-state--current-namespace)))
                               (list pod-name exec-command)))

         (interactive-tty (member "-t" args))
         (buf
          (if interactive-tty
              (kubernetes-process-term-buffer-start kubernetes-exec-buffer-name
                                                    kubernetes-kubectl-executable
                                                    command-args)
            (kubernetes-process-buffer-start kubernetes-exec-buffer-name
                                             #'kubernetes-mode
                                             kubernetes-kubectl-executable
                                             command-args))))

    (when (and interactive-tty kubernetes-clean-up-interactive-exec-buffers)
      (set-process-sentinel (get-buffer-process buf) #'kubernetes-process-kill-quietly))

    (select-window (display-buffer buf))))


(magit-define-popup kubernetes-config-popup
  "Popup console for showing an overview of available config commands."
  :group 'kubernetes
  :actions
  '("Managing contexts"
    (?c "Change context" kubernetes-use-context)
    "Query settings"
    (?n "Set namespace" kubernetes-set-namespace)))

(defun kubernetes-set-namespace (ns)
  "Set the namespace to query to NS, overriding the settings for the current context."
  (interactive (list (completing-read "Use namespace: " (kubernetes--namespace-names) nil t)))
  ;; The context is safe to preserve, but everything else should be reset.
  (let ((context kubernetes-state--view-config-response))
    (kubernetes-state-kill-polling-processes)
    (kubernetes-state-clear)
    (goto-char (point-min))
    (setq kubernetes-state--view-config-response context)
    (setq kubernetes-state--current-namespace ns)
    (kubernetes--redraw-buffers t)))

(defun kubernetes--namespace-names ()
  (-let* ((config (or kubernetes-state--get-namespaces-response (kubernetes-kubectl-await-on-async #'kubernetes-kubectl-get-namespaces)))
          ((&alist 'items items) config))
    (-map (-lambda ((&alist 'metadata (&alist 'name name))) name) items)))

(defun kubernetes-use-context (context)
  "Switch Kubernetes context refresh the pods buffer.

CONTEXT is the name of a context as a string."
  (interactive (list (completing-read "Context: " (kubernetes--context-names) nil t)))
  (kubernetes-state-kill-polling-processes)
  (kubernetes-state-clear)
  (kubernetes--redraw-buffers t)
  (goto-char (point-min))
  (kubernetes-kubectl-config-use-context context (lambda (_)
                                                   (kubernetes-refresh))))

(defun kubernetes--context-names ()
  (-let* ((config (or kubernetes-state--view-config-response (kubernetes-kubectl-await-on-async #'kubernetes-kubectl-config-view)))
          ((&alist 'contexts contexts) config))
    (--map (alist-get 'name it) contexts)))


(magit-define-popup kubernetes-overview-popup
  "Popup console for showing an overview of available popup commands."
  :group 'kubernetes
  :actions
  '("Environment"
    (?c "Configuration" kubernetes-config-popup)
    "Marking pods"
    (?D "Delete pod at point" kubernetes-mark-for-delete)
    (?u "Unmark pod at point" kubernetes-unmark)
    (?U "Unmark all pods" kubernetes-unmark-all)
    "Popup commands"
    (?d "Describe" kubernetes-describe-popup)
    (?e "Exec" kubernetes-exec-popup)
    (?l "Logs" kubernetes-logs-popup)
    "Misc"
    (?h "Describe mode and keybindings" describe-mode)))


;; Mode definitions

;;;###autoload
(defvar kubernetes-mode-map
  (let ((keymap (make-sparse-keymap)))
    ;; Section controls
    (define-key keymap (kbd "p")   #'magit-section-backward)
    (define-key keymap (kbd "n")   #'magit-section-forward)
    (define-key keymap (kbd "M-p") #'magit-section-backward-sibling)
    (define-key keymap (kbd "M-n") #'magit-section-forward-sibling)
    (define-key keymap (kbd "C-i") #'magit-section-toggle)
    (define-key keymap (kbd "^")   #'magit-section-up)
    (define-key keymap [tab]       #'magit-section-toggle)
    (define-key keymap [C-tab]     #'magit-section-cycle)
    (define-key keymap [M-tab]     #'magit-section-cycle-diffs)
    (define-key keymap [S-tab]     #'magit-section-cycle-global)
    ;; Misc
    (define-key keymap (kbd "q") #'quit-window)
    (define-key keymap (kbd "RET") #'kubernetes-navigate)
    (define-key keymap (kbd "M-w") #'kubernetes-copy-thing-at-point)

    keymap)
  "Keymap for `kubernetes-mode'.  This is the base keymap for all derived modes.")

;;;###autoload
(define-derived-mode kubernetes-mode special-mode "Kubernetes"
  "Base mode for Kubernetes modes.

\\{kubernetes-mode-map}"
  :group 'kubernetes
  (read-only-mode)
  (buffer-disable-undo)
  (setq truncate-lines t)
  (setq-local line-move-visual t)
  (setq show-trailing-whitespace nil)
  (setq list-buffers-directory (abbreviate-file-name default-directory))
  (hack-dir-local-variables-non-file-buffer)
  (make-local-variable 'text-property-default-nonsticky)
  (push (cons 'keymap t) text-property-default-nonsticky)
  (push (cons 'kubernetes-nav t) text-property-default-nonsticky)
  (push (cons 'kubernetes-copy t) text-property-default-nonsticky)
  (add-hook 'post-command-hook #'magit-section-update-highlight t t)
  (setq-local redisplay-highlight-region-function 'magit-highlight-region)
  (setq-local redisplay-unhighlight-region-function 'magit-unhighlight-region)
  (when (bound-and-true-p global-linum-mode)
    (linum-mode -1))
  (when (and (fboundp 'nlinum-mode)
             (bound-and-true-p global-nlinum-mode))
    (nlinum-mode -1)))

;;;###autoload
(defvar kubernetes-display-pods-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "?") #'kubernetes-overview-popup)
    (define-key keymap (kbd "c") #'kubernetes-config-popup)
    (define-key keymap (kbd "d") #'kubernetes-describe-popup)
    (define-key keymap (kbd "D") #'kubernetes-mark-for-delete)
    (define-key keymap (kbd "e") #'kubernetes-exec-popup)
    (define-key keymap (kbd "g") #'kubernetes-refresh)
    (define-key keymap (kbd "u") #'kubernetes-unmark)
    (define-key keymap (kbd "U") #'kubernetes-unmark-all)
    (define-key keymap (kbd "x") #'kubernetes-execute-marks)
    (define-key keymap (kbd "l") #'kubernetes-logs-popup)
    (define-key keymap (kbd "h") #'describe-mode)
    keymap)
  "Keymap for `kubernetes-display-pods-mode'.")

;;;###autoload
(define-derived-mode kubernetes-display-pods-mode kubernetes-mode "Kubernetes Pods"
  "Mode for working with Kubernetes pods.

\\<kubernetes-display-pods-mode-map>\
Type \\[kubernetes-mark-for-delete] to mark a pod for deletion, and \\[kubernetes-execute-marks] to execute.
Type \\[kubernetes-unmark] to unmark the pod at point, or \\[kubernetes-unmark-all] to unmark all pods.

Type \\[kubernetes-navigate] to inspect the object on the current line.

Type \\[kubernetes-exec] to exec into a pod.

Type \\[kubernetes-logs] when point is on a pod to view its logs.

Type \\[kubernetes-copy-thing-at-point] to copy the pod name at point.

Type \\[kubernetes-refresh] to refresh the buffer.

\\{kubernetes-display-pods-mode-map}"
  :group 'kubernetes)

;;;###autoload
(defvar kubernetes-display-configmaps-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "?") #'kubernetes-overview-popup)
    (define-key keymap (kbd "c") #'kubernetes-config-popup)
    (define-key keymap (kbd "d") #'kubernetes-describe-popup)
    (define-key keymap (kbd "D") #'kubernetes-mark-for-delete)
    (define-key keymap (kbd "g") #'kubernetes-refresh)
    (define-key keymap (kbd "u") #'kubernetes-unmark)
    (define-key keymap (kbd "U") #'kubernetes-unmark-all)
    (define-key keymap (kbd "x") #'kubernetes-execute-marks)
    (define-key keymap (kbd "h") #'describe-mode)
    keymap)
  "Keymap for `kubernetes-display-configmaps-mode'.")

;;;###autoload
(define-derived-mode kubernetes-display-configmaps-mode kubernetes-mode "Kubernetes Configmaps"
  "Mode for working with Kubernetes configmaps.

\\<kubernetes-display-configmaps-mode-map>\
Type \\[kubernetes-mark-for-delete] to mark a configmap for deletion, and \\[kubernetes-execute-marks] to execute.
Type \\[kubernetes-unmark] to unmark the configmap at point, or \\[kubernetes-unmark-all] to unmark all configmaps.

Type \\[kubernetes-navigate] to inspect the object on the current line.

Type \\[kubernetes-copy-thing-at-point] to copy the configmap name at point.

Type \\[kubernetes-refresh] to refresh the buffer.

\\{kubernetes-display-configmaps-mode-map}"
  :group 'kubernetes)

;;;###autoload
(defvar kubernetes-display-secrets-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "?") #'kubernetes-overview-popup)
    (define-key keymap (kbd "c") #'kubernetes-config-popup)
    (define-key keymap (kbd "d") #'kubernetes-describe-popup)
    (define-key keymap (kbd "D") #'kubernetes-mark-for-delete)
    (define-key keymap (kbd "g") #'kubernetes-refresh)
    (define-key keymap (kbd "u") #'kubernetes-unmark)
    (define-key keymap (kbd "U") #'kubernetes-unmark-all)
    (define-key keymap (kbd "x") #'kubernetes-execute-marks)
    (define-key keymap (kbd "h") #'describe-mode)
    keymap)
  "Keymap for `kubernetes-display-secrets-mode'.")

;;;###autoload
(define-derived-mode kubernetes-display-secrets-mode kubernetes-mode "Kubernetes Secrets"
  "Mode for working with Kubernetes secrets.

\\<kubernetes-display-secrets-mode-map>\
Type \\[kubernetes-mark-for-delete] to mark a secret for deletion, and \\[kubernetes-execute-marks] to execute.
Type \\[kubernetes-unmark] to unmark the secret at point, or \\[kubernetes-unmark-all] to unmark all secrets.

Type \\[kubernetes-navigate] to inspect the object on the current line.

Type \\[kubernetes-copy-thing-at-point] to copy the secret name at point.

Type \\[kubernetes-refresh] to refresh the buffer.

\\{kubernetes-display-secrets-mode-map}"
  :group 'kubernetes)

;;;###autoload
(defvar kubernetes-logs-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "n") #'kubernetes-logs-forward-line)
    (define-key keymap (kbd "p") #'kubernetes-logs-previous-line)
    (define-key keymap (kbd "RET") #'kubernetes-logs-inspect-line)
    keymap)
  "Keymap for `kubernetes-logs-mode'.")

;;;###autoload
(define-derived-mode kubernetes-logs-mode kubernetes-mode "Kubernetes Logs"
  "Mode for displaying and inspecting Kubernetes logs.

\\<kubernetes-logs-mode-map>\
Type \\[kubernetes-logs-inspect-line] to open the line at point in a new buffer.

\\{kubernetes-logs-mode-map}")

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

;;;###autoload
(define-derived-mode kubernetes-display-thing-mode kubernetes-mode "Kubernetes Object"
  "Mode for inspecting a Kubernetes object.

\\{kubernetes-display-thing-mode-map}"
  :group 'kubernetes)

;;;###autoload
(defvar kubernetes-overview-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "?") #'kubernetes-overview-popup)
    (define-key keymap (kbd "c") #'kubernetes-config-popup)
    (define-key keymap (kbd "d") #'kubernetes-describe-popup)
    (define-key keymap (kbd "D") #'kubernetes-mark-for-delete)
    (define-key keymap (kbd "e") #'kubernetes-exec-popup)
    (define-key keymap (kbd "g") #'kubernetes-refresh)
    (define-key keymap (kbd "l") #'kubernetes-logs-popup)
    (define-key keymap (kbd "u") #'kubernetes-unmark)
    (define-key keymap (kbd "U") #'kubernetes-unmark-all)
    (define-key keymap (kbd "x") #'kubernetes-execute-marks)
    (define-key keymap (kbd "h") #'describe-mode)
    keymap)
  "Keymap for `kubernetes-overview-mode'.")

;;;###autoload
(define-derived-mode kubernetes-overview-mode kubernetes-mode "Kubernetes Overview"
  "Mode for working with Kubernetes overview.

\\<kubernetes-overview-mode-map>\
Type \\[kubernetes-mark-for-delete] to mark an object for deletion, and \\[kubernetes-execute-marks] to execute.
Type \\[kubernetes-unmark] to unmark the object at point, or \\[kubernetes-unmark-all] to unmark all objects.

Type \\[kubernetes-navigate] to inspect the object on the current line.

Type \\[kubernetes-copy-thing-at-point] to copy the thing at point.

Type \\[kubernetes-refresh] to refresh the buffer.

\\{kubernetes-overview-mode-map}"
  :group 'kubernetes)


;; Main entrypoints.

;;;###autoload
(defun kubernetes-display-pods ()
  "Display a list of pods in the current Kubernetes context."
  (interactive)
  (kubernetes-display-buffer (kubernetes--display-pods-initialize-buffer))
  (message (substitute-command-keys "\\<kubernetes-display-pods-mode-map>Type \\[kubernetes-overview-popup] for usage.")))

;;;###autoload
(defun kubernetes-display-configmaps ()
  "Display a list of configmaps in the current Kubernetes context."
  (interactive)
  (kubernetes-display-buffer (kubernetes--display-configmaps-initialize-buffer))
  (message (substitute-command-keys "\\<kubernetes-display-configmaps-mode-map>Type \\[kubernetes-overview-popup] for usage.")))

;;;###autoload
(defun kubernetes-display-secrets ()
  "Display a list of secrets in the current Kubernetes context."
  (interactive)
  (kubernetes-display-buffer (kubernetes--display-secrets-initialize-buffer))
  (message (substitute-command-keys "\\<kubernetes-display-secrets-mode-map>Type \\[kubernetes-overview-popup] for usage.")))


;; Overview

(defun kubernetes--overview-initialize-buffer ()
  "Called the first time the overview buffer is opened to set up the buffer."
  (let ((buf (get-buffer-create kubernetes-overview-buffer-name)))
    (with-current-buffer buf
      (kubernetes-overview-mode)

      ;; Render buffer.
      (kubernetes--redraw-overview-buffer t)
      (goto-char (point-min))

      (kubernetes-state-initialize-timers)
      (add-hook 'kill-buffer-hook (kubernetes--make-cleanup-fn buf) nil t))
    buf))

(defun kubernetes--redraw-overview-buffer (&optional force)
  "Redraws the main buffer using the current state.

FORCE ensures it happens."
  (when-let (buf (get-buffer kubernetes-overview-buffer-name))
    (with-current-buffer buf
      (when (or force
                ;; HACK: Only redraw the buffer if it is in the selected window.
                ;;
                ;; The cursor moves unpredictably in a redraw, which ruins the current
                ;; position in the buffer if a popup window is open.
                (equal (window-buffer) buf))

        (kubernetes-state-clear-error-if-stale)

        (let ((pos (point))
              (inhibit-read-only t)
              (inhibit-redisplay t)
              (state (kubernetes--state)))

          (erase-buffer)
          (kubernetes-ast-eval `(section (root nil)
                               ,(kubernetes-errors-render state)
                               ,(kubernetes-contexts-render state)
                               ,(kubernetes-configmaps-render state t)
                               ,(kubernetes-pods-render state t)
                               ,(kubernetes-secrets-render state t)
                               ,(kubernetes-services-render state t)))
          (goto-char pos)))

      ;; Force the section at point to highlight.
      (magit-section-update-highlight))))


;;;###autoload
(defun kubernetes-overview ()
  "Display an overview buffer for Kubernetes."
  (interactive)
  (kubernetes-display-buffer (kubernetes--overview-initialize-buffer))
  (message (substitute-command-keys "\\<kubernetes-overview-mode-map>Type \\[kubernetes-overview-popup] for usage.")))

(provide 'kubernetes)

;;; kubernetes.el ends here
