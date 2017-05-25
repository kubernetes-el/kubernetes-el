;;; kubernetes-mode.el --- Major mode for kubernetes buffers.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'magit)
(require 'kubernetes-props)

(defconst kubernetes-mode-props
  '((linum-mode . linum-mode)
    (nlinum-mode . nlinum-mode))
  "Alist of functions to inject for testing and isolation.")

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

    (define-key keymap (kbd "q") #'quit-window)
    (define-key keymap (kbd "h") #'describe-mode)

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
