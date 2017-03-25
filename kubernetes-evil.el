;;; kubernetes-evil.el --- Keybindings for evil-mode.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(require 'evil)

(autoload 'kubernetes-display-pods-mode-map "kubernetes")
(autoload 'kubernetes-display-pod-mode-map "kubernetes")

(evil-define-key 'normal kubernetes-display-pods-mode-map
  (kbd "RET") #'kubernetes-navigate
  (kbd "g") #'kubernetes-display-pods-refresh
  (kbd "q") #'quit-window
  (kbd "d") #'kubernetes-mark-for-delete
  (kbd "u") #'kubernetes-unmark
  (kbd "U") #'kubernetes-unmark-all
  (kbd "x") #'kubernetes-execute-marks)

(evil-define-key 'normal kubernetes-display-pod-mode-map
  (kbd "RET") #'kubernetes-navigate
  (kbd "q") #'quit-window)

(provide 'kubernetes-evil)

;;; kubernetes-evil.el ends here
