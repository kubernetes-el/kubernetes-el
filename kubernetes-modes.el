;;; kubernetes-modes.el --- Base modes for Kubernetes.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'magit)
(require 'subr-x)

(autoload 'kubernetes-config-popup "kubernetes-popups")
(autoload 'kubernetes-copy-thing-at-point "kubernetes-commands")
(autoload 'kubernetes-describe-popup "kubernetes-popups")
(autoload 'kubernetes-exec-popup "kubernetes-popups")
(autoload 'kubernetes-execute-marks "kubernetes-commands")
(autoload 'kubernetes-labels-popup "kubernetes-popups")
(autoload 'kubernetes-logs-popup "kubernetes-popups")
(autoload 'kubernetes-mark-for-delete "kubernetes-commands")
(autoload 'kubernetes-navigate "kubernetes-commands")
(autoload 'kubernetes-overview-popup "kubernetes-popups")
(autoload 'kubernetes-refresh "kubernetes-commands")
(autoload 'kubernetes-unmark "kubernetes-commands")
(autoload 'kubernetes-unmark-all "kubernetes-commands")

;;;###autoload
(define-derived-mode kubernetes-display-thing-mode kubernetes-mode "Kubernetes Object"
  "Mode for inspecting a Kubernetes object.

\\{kubernetes-display-thing-mode-map}"
  :group 'kubernetes)

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
    (define-key keymap (kbd "h") #'describe-mode)

    (define-key keymap (kbd "?") #'kubernetes-overview-popup)
    (define-key keymap (kbd "c") #'kubernetes-config-popup)
    (define-key keymap (kbd "d") #'kubernetes-describe-popup)
    (define-key keymap (kbd "D") #'kubernetes-mark-for-delete)
    (define-key keymap (kbd "e") #'kubernetes-exec-popup)
    (define-key keymap (kbd "g") #'kubernetes-refresh)
    (define-key keymap (kbd "l") #'kubernetes-logs-popup)
    (define-key keymap (kbd "L") #'kubernetes-labels-popup)
    (define-key keymap (kbd "u") #'kubernetes-unmark)
    (define-key keymap (kbd "U") #'kubernetes-unmark-all)
    (define-key keymap (kbd "x") #'kubernetes-execute-marks)

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

(provide 'kubernetes-modes)

;;; kubernetes-modes.el ends here
