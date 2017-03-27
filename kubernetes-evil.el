;;; kubernetes-evil.el --- Keybindings for evil-mode.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(require 'evil)

(autoload 'kubernetes-mode-map "kubernetes")
(autoload 'kubernetes-display-pods-mode-map "kubernetes")
(autoload 'kubernetes-logs-mode-map "kubernetes")
(autoload 'kubernetes-log-line-mode-map "kubernetes")

(evil-define-key 'normal kubernetes-mode-map
  (kbd "RET") #'kubernetes-navigate
  (kbd "q") #'quit-window
  (kbd "M-w") #'kubernetes-copy-thing-at-point)

(evil-define-key 'normal kubernetes-display-pods-mode-map
  (kbd "g") #'kubernetes-display-pods-refresh
  (kbd "h") #'describe-mode
  (kbd "d") #'kubernetes-describe-pod
  (kbd "D") #'kubernetes-mark-for-delete
  (kbd "u") #'kubernetes-unmark
  (kbd "U") #'kubernetes-unmark-all
  (kbd "x") #'kubernetes-execute-marks
  (kbd "l") #'kubernetes-logs)

(evil-define-key 'normal kubernetes-logs-mode-map
  (kbd "n") #'kubernetes-logs-forward-line
  (kbd "p") #'kubernetes-logs-previous-line
  (kbd "RET") #'kubernetes-logs-inspect-line)

(evil-define-key 'normal kubernetes-log-line-mode-map
  (kbd "n") #'kubernetes-logs-forward-line
  (kbd "p") #'kubernetes-logs-previous-line)

(provide 'kubernetes-evil)

;;; kubernetes-evil.el ends here
