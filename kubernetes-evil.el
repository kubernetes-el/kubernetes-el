;;; kubernetes-evil.el --- Keybindings for evil-mode.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'evil nil t))

(autoload 'evil-delay "evil-common")
(autoload 'evil-set-initial-state "evil-core")
(autoload 'magit-section-toggle "magit-section")

(with-eval-after-load 'evil
  (evil-set-initial-state 'kubernetes-mode 'motion)
  (evil-set-initial-state 'kubernetes-display-pods-mode 'motion)
  (evil-set-initial-state 'kubernetes-display-thing-mode 'motion)
  (evil-set-initial-state 'kubernetes-log-line-mode 'motion)
  (evil-set-initial-state 'kubernetes-logs-mode 'motion)

  (evil-define-key 'motion kubernetes-mode-map
    (kbd "RET") #'kubernetes-navigate
    (kbd "j") #'next-line
    (kbd "k") #'previous-line
    (kbd "q") #'quit-window
    (kbd "M-w") #'kubernetes-copy-thing-at-point)

  (evil-define-key 'motion kubernetes-display-pods-mode-map
    (kbd "?") #'kubernetes-overview-popup
    (kbd "TAB") #'magit-section-toggle
    (kbd "c") #'kubernetes-config-popup
    (kbd "g r") #'kubernetes-refresh
    (kbd "h") #'describe-mode
    (kbd "d") #'kubernetes-describe
    (kbd "D") #'kubernetes-mark-for-delete
    (kbd "e") #'kubernetes-exec
    (kbd "u") #'kubernetes-unmark
    (kbd "U") #'kubernetes-unmark-all
    (kbd "x") #'kubernetes-execute-marks
    (kbd "l") #'kubernetes-logs)

  (evil-define-key 'motion kubernetes-logs-mode-map
    (kbd "n") #'kubernetes-logs-forward-line
    (kbd "p") #'kubernetes-logs-previous-line
    (kbd "RET") #'kubernetes-logs-inspect-line)

  (evil-define-key 'motion kubernetes-log-line-mode-map
    (kbd "n") #'kubernetes-logs-forward-line
    (kbd "p") #'kubernetes-logs-previous-line))

(provide 'kubernetes-evil)

;;; kubernetes-evil.el ends here
