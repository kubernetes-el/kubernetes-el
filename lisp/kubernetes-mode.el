;;; kubernetes-mode.el --- Major mode for kubernetes buffers.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'magit)
(require 'kubernetes-props)

(autoload 'kubernetes-config-popup "kubernetes-config")
(autoload 'kubernetes-overview-popup "kubernetes-overview")

(defconst kubernetes-mode-props
  '((linum-mode . linum-mode)
    (nlinum-mode . nlinum-mode))
  "Alist of functions to inject for testing and isolation.")

(defun kubernetes--ellipsize (s threshold)
  (if (> (length s) threshold)
      (concat (substring s 0 (1- threshold)) "…")
    s))

;;;###autoload
(defun kubernetes-copy-thing-at-point (point)
  "Perform a context-sensitive copy action.

Inspecs the `kubernetes-copy' text property at POINT to determine
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
      (let ((ellipsized (kubernetes--ellipsize first-line 70)))
        (if (< 1 n-lines)
            (message "Copied %s lines, starting with: %s" n-lines ellipsized)
          (message "Copied: %s" ellipsized))))))

;;;###autoload
(defconst kubernetes-mode-map
  (let ((keymap (make-sparse-keymap)))
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

    (define-key keymap (kbd "M-w") #'kubernetes-copy-thing-at-point)

    (define-key keymap (kbd "q") #'quit-window)
    (define-key keymap (kbd "h") #'describe-mode)

    (define-key keymap (kbd "c") #'kubernetes-config-popup)
    (define-key keymap (kbd "o") #'kubernetes-overview-popup)

    keymap)
  "Keymap for `kubernetes-mode'.")

;;;###autoload
(define-derived-mode kubernetes-mode special-mode "Kubernetes"
  "Mode for kubernetes overview buffer.

\\{kubernetes-mode-map}"
  :group 'kubernetes

  (kubernetes-props-bind ([linum-mode nlinum-mode] kubernetes-mode-props)
    (read-only-mode)
    (buffer-disable-undo)
    (setq truncate-lines t)
    (setq-local line-move-visual t)
    (setq show-trailing-whitespace nil)
    (setq list-buffers-directory (abbreviate-file-name default-directory))
    (hack-dir-local-variables-non-file-buffer)
    (add-hook 'post-command-hook #'magit-section-update-highlight t t)
    (setq-local redisplay-highlight-region-function 'magit-highlight-region)
    (setq-local redisplay-unhighlight-region-function 'magit-unhighlight-region)

    (when (bound-and-true-p global-linum-mode)
      (linum-mode -1))
    (when (bound-and-true-p global-nlinum-mode)
      (nlinum-mode -1))))

(provide 'kubernetes-mode)

;;; kubernetes-mode.el ends here
