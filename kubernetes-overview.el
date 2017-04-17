;;; kubernetes-overview.el --- Utilities for managing the overview buffer. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'subr-x)

(require 'kubernetes-commands)
(require 'kubernetes-configmaps)
(require 'kubernetes-contexts)
(require 'kubernetes-deployments)
(require 'kubernetes-errors)
(require 'kubernetes-modes)
(require 'kubernetes-namespaces)
(require 'kubernetes-pods)
(require 'kubernetes-popups)
(require 'kubernetes-secrets)
(require 'kubernetes-services)
(require 'kubernetes-timers)

;; Component

(defun kubernetes-overview-render (state)
  `(section (root nil)
            ,(kubernetes-errors-render state)
            ,(kubernetes-contexts-render state)
            ,(kubernetes-configmaps-render state t)
            ,(kubernetes-deployments-render state t)
            ,(kubernetes-pods-render state t)
            ,(kubernetes-secrets-render state t)
            ,(kubernetes-services-render state t)))


;; Overview buffer.

(defun kubernetes-overview--redraw-buffer ()
  "Redraws the main buffer using the current state."
  (when-let (buf (get-buffer kubernetes-overview-buffer-name))
    (with-current-buffer buf
      ;; HACK: Only redraw the buffer if it is in the selected window.
      ;;
      ;; The cursor moves unpredictably in a redraw, which ruins the current
      ;; position in the buffer if a popup window is open.
      (when (equal (get-buffer-window buf)
                   (selected-window))
        (kubernetes-utils--save-window-state
         (let ((inhibit-read-only t))
           (erase-buffer)
           (kubernetes-ast-eval (kubernetes-overview-render (kubernetes-state)))))

        ;; Force the section at point to highlight.
        (magit-section-update-highlight)))))

(defun kubernetes-overview--poll (&optional verbose)
  (kubernetes-configmaps-refresh verbose)
  (kubernetes-contexts-refresh verbose)
  (kubernetes-deployments-refresh verbose)
  (kubernetes-namespaces-refresh verbose)
  (kubernetes-pods-refresh verbose)
  (kubernetes-secrets-refresh verbose)
  (kubernetes-services-refresh verbose))

(defun kubernetes-overview--initialize-buffer ()
  "Called the first time the overview buffer is opened to set up the buffer."
  (let ((buf (get-buffer-create kubernetes-overview-buffer-name)))
    (with-current-buffer buf
      (kubernetes-overview-mode)
      (add-hook 'kubernetes-redraw-hook #'kubernetes-overview--redraw-buffer)
      (add-hook 'kubernetes-poll-hook #'kubernetes-overview--poll)
      (kubernetes-timers-initialize-timers)
      (kubernetes-overview--redraw-buffer)
      (add-hook 'kill-buffer-hook (kubernetes-utils-make-cleanup-fn buf) nil t))
    buf))

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

;;;###autoload
(defun kubernetes-overview ()
  "Display an overview buffer for Kubernetes."
  (interactive)
  (kubernetes-commands-display-buffer (kubernetes-overview--initialize-buffer))
  (message (substitute-command-keys "\\<kubernetes-overview-mode-map>Type \\[kubernetes-overview-popup] for usage.")))


(provide 'kubernetes-overview)

;;; kubernetes-overview.el ends here
