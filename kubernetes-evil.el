;;; kubernetes-evil.el --- Keybindings for evil-mode.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(require 'evil)

(autoload 'kubernetes-display-pods-mode-map "kubernetes")

(evil-define-key 'normal kubernetes-display-pods-mode-map
  (kbd "TAB") #'magit-section-toggle
  (kbd "g") #'kubernetes-display-pods-refresh
  (kbd "q") #'quit-window)

(provide 'kubernetes-evil)

;;; kubernetes-evil.el ends here
