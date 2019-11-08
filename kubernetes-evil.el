;;; kubernetes-evil.el --- Kubernetes keybindings for evil-mode.

;; Copyright (C) 2017  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;; Version: 0.13.0

;; Package-Requires: ((kubernetes "0.13.0") (evil "1.2.12"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides Evil bindings for Kubernetes buffers.

;;; Code:

(require 'evil)
(require 'kubernetes)

(evil-set-initial-state 'kubernetes-mode 'motion)
(evil-set-initial-state 'kubernetes-display-thing-mode 'motion)
(evil-set-initial-state 'kubernetes-log-line-mode 'motion)
(evil-set-initial-state 'kubernetes-logs-mode 'motion)
(evil-set-initial-state 'kubernetes-overview-mode 'motion)

(evil-define-key 'motion kubernetes-mode-map
  (kbd "p")   #'magit-section-backward
  (kbd "n")   #'magit-section-forward
  (kbd "M-p") #'magit-section-backward-sibling
  (kbd "M-n") #'magit-section-forward-sibling
  (kbd "C-i") #'magit-section-toggle
  (kbd "^")   #'magit-section-up
  [tab]       #'magit-section-toggle
  [C-tab]     #'magit-section-cycle
  [M-tab]     #'magit-section-cycle-diffs
  [S-tab]     #'magit-section-cycle-global

  [remap evil-next-line] #'next-line
  [remap evil-previous-line] #'previous-line
  [remap evil-next-visual-line] #'next-line
  [remap evil-previous-visual-line] #'previous-line

  (kbd "q") #'quit-window
  (kbd "RET") #'kubernetes-navigate
  (kbd "M-w") #'kubernetes-copy-thing-at-point

  (kbd "?") #'kubernetes-overview-popup
  (kbd "c") #'kubernetes-config-popup
  (kbd "g r") #'kubernetes-refresh
  (kbd "h") #'describe-mode
  (kbd "d") #'kubernetes-describe-popup
  (kbd "D") #'kubernetes-mark-for-delete
  (kbd "e") #'kubernetes-exec-popup
  (kbd "u") #'kubernetes-unmark
  (kbd "U") #'kubernetes-unmark-all
  (kbd "x") #'kubernetes-execute-marks
  (kbd "l") #'kubernetes-logs-popup
  (kbd "L") #'kubernetes-labels-popup)

(evil-define-key 'motion kubernetes-overview-mode-map
  (kbd "v") #'kubernetes-overview-set-sections)

(evil-define-key 'motion kubernetes-logs-mode-map
  (kbd "n") #'kubernetes-logs-forward-line
  (kbd "p") #'kubernetes-logs-previous-line
  (kbd "RET") #'kubernetes-logs-inspect-line)

(evil-define-key 'motion kubernetes-log-line-mode-map
  (kbd "n") #'kubernetes-logs-forward-line
  (kbd "p") #'kubernetes-logs-previous-line)

(provide 'kubernetes-evil)

;;; kubernetes-evil.el ends here
