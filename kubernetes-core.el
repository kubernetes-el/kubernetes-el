;; kubernetes-core.el --- core functionality -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'kubernetes-vars)

(defun kubernetes--redraw-overview-buffer ()
  "Redraws the main buffer using the current state."
  (when-let (buf (get-buffer kubernetes-overview-buffer-name))
    (with-current-buffer buf
      ;; If a region is active, a redraw would affect the region in
      ;; unpredictable ways.
      (unless (region-active-p)
        ;; Suppress redrawing if the overview is not selected. This prevents
        ;; point from jumping around when a magit popup is open.
        (when (member (selected-window) (get-buffer-window-list buf))
          (kubernetes-utils--save-window-state
           (let ((inhibit-read-only t))
             (erase-buffer)
             (kubernetes-ast-eval (kubernetes-overview-render (kubernetes-state)))))

          ;; Force the section at point to highlight.
          (magit-section-update-highlight))))))

;; Shamelessly copied from emacs-lsp/lsp-mode
(defun kubernetes--message (format &rest args)
  "Call `message' with FORMAT and ARGS.

We `inhibit-message' the message when the cursor is in the
minibuffer and when Emacs version is before Emacs 27 due to the
fact that we often use `kubernetes--info', `kubernetes--warn' and
`kubernetes--error' in async context and the call to these
function is removing the minibuffer prompt.  The issue with async
messages is already fixed in Emacs 27."
  (when kubernetes-show-message
    (let ((inhibit-message (and (minibufferp)
                                (version< emacs-version "27.0"))))
      (apply #'message format args))))

;; Shamelessly copied from emacs-lsp/lsp-mode
(defun kubernetes--info (format &rest args)
  "Display kubernetes info message with FORMAT with ARGS."
  (kubernetes--message "%s :: %s" (propertize "k8s" 'face 'success) (apply #'format format args)))

;; Shamelessly copied from emacs-lsp/lsp-mode
(defun kubernetes--warn (format &rest args)
  "Display kubernetes warn message with FORMAT with ARGS."
  (kubernetes--message "%s :: %s" (propertize "k8s" 'face 'warning) (apply #'format format args)))

;; Shamelessly copied from emacs-lsp/lsp-mode
(defun kubernetes--error (format &rest args)
  "Display kubernetes error message with FORMAT with ARGS."
  (kubernetes--message "%s :: %s" (propertize "k8s" 'face 'error) (apply #'format format args)))

(provide 'kubernetes-core)
;;; kubernetes-core.el ends here
