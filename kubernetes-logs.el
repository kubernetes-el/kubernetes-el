;;; kubernetes-logs.el --- Utilities for working with log buffers.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'subr-x)

(require 'kubernetes-modes)
(require 'kubernetes-popups)
(require 'kubernetes-utils)

(autoload 'json-pretty-print-buffer "json")

(require 'kubernetes-vars)

(defun kubernetes-logs--log-line-buffer-for-string (s)
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
  (with-current-buffer kubernetes-logs-buffer-name
    (forward-line -1)
    (when (get-buffer kubernetes-log-line-buffer-name)
      (kubernetes-logs-inspect-line (point)))))

;;;###autoload
(defun kubernetes-logs-forward-line ()
  "Move forward and inspect the line at point."
  (interactive)
  (with-current-buffer kubernetes-logs-buffer-name
    (forward-line 1)
    (when (get-buffer kubernetes-log-line-buffer-name)
      (kubernetes-logs-inspect-line (point)))))

;;;###autoload
(defun kubernetes-logs-follow (pod-name args state)
  "Open a streaming logs buffer for a pod.

POD-NAME is the name of the pod to log.

ARGS are additional args to pass to kubectl.

STATE is the current application state."
  (interactive
   (let ((state (kubernetes-state)))
     (list (or (kubernetes-utils-maybe-pod-name-at-point)
               (kubernetes-utils-read-pod-name state))
           (kubernetes-logs-arguments)
           state)))
  (kubernetes-logs-fetch-all pod-name (cons "-f" args) state))

;;;###autoload
(defun kubernetes-logs-fetch-all (pod-name args state)
  "Open a streaming logs buffer for POD.

POD-NAME is the name of the pod to log.

ARGS are additional args to pass to kubectl.

STATE is the current application state"
  (interactive
   (let ((state (kubernetes-state)))
     (list (or (kubernetes-utils-maybe-pod-name-at-point) (kubernetes-utils-read-pod-name state))
           (kubernetes-logs-arguments)
           state)))
  (let ((args (append (list "logs") args (list pod-name) (kubernetes-kubectl--flags-from-state (kubernetes-state))
                      (when-let (ns (kubernetes-state-current-namespace state))
                        (list (format "--namespace=%s" ns))))))
    (with-current-buffer (kubernetes-utils-process-buffer-start kubernetes-logs-buffer-name #'kubernetes-logs-mode kubernetes-kubectl-executable args)
      (select-window (display-buffer (current-buffer))))))


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

(provide 'kubernetes-logs)

;;; kubernetes-logs.el ends here
