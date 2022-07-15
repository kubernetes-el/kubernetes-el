;;; kubernetes-el-tramp.el --- Tramp setup for kubernetes-el.  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Erik Hetzner

;; Author: Erik Hetzner <egh@e6h.org>
;; Maintainer: Erik Hetzner <egh@e6h.org>
;;     Noorul Islam K M <noorul@noorul.com>
;;     Jonathan Jin <me@jonathanj.in>

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

;; This file is named kubernetes-el-tramp to avoid a conflict with
;; kubernetes-tramp (https://github.com/gruggiero/kubernetes-tramp)

;;; Code:

(require 'dash)
(require 'tramp)

(require 'kubernetes-pods)

(defun kubernetes-tramp--running-containers (&optional _ignored)
  "A tramp-completion function for kubernetes."
  (if (null (kubernetes-state))
      (kubernetes-pods-refresh-now))
  (-let (((&alist 'items pods) (kubernetes-state--get (kubernetes-state) 'pods)))
    (-map (-lambda ((&alist 'metadata (&alist 'name)))
            (list nil name))
          pods)))

(defun get--tramp-file-path (pod-name &optional container-name)
  "Return TRAMP file path formed using POD-NAME and/or CONTAINER-NAME."
  (if container-name
      (format "/kubernetes:%s@%s:" container-name pod-name)
    (format "/kubernetes:%s:" pod-name)))

(defun kubernetes-tramp-find-file (pod-name container-name)
  (interactive (let* ((state (kubernetes-state))
                      (pod-name (or (kubernetes-utils-maybe-pod-name-at-point) (kubernetes-pods--read-name state)))
                      (container-name (kubernetes--val-from-arg-list (transient-args 'kubernetes-file) 'container)))
                 (list pod-name container-name)))
  (let ((default-directory (get--tramp-file-path pod-name container-name)))
    (call-interactively #'find-file)))

(defun kubernetes-tramp-dired (pod-name container-name)
  (interactive (let* ((state (kubernetes-state))
                      (pod-name (or (kubernetes-utils-maybe-pod-name-at-point) (kubernetes-pods--read-name state)))
                      (container-name (kubernetes--val-from-arg-list (transient-args 'kubernetes-file) 'container)))
                 (list pod-name container-name)))
  (dired (get--tramp-file-path pod-name container-name)))

(add-to-list 'tramp-methods
             `("kubernetes"
               (tramp-login-program ,kubernetes-kubectl-executable)
               (tramp-login-args (("exec" "-i" "-t") ("-c" "%u") ("%h") ("--" "%l")))
               (tramp-remote-shell "/bin/sh")
               (tramp-remote-shell-login ("-l"))
               (tramp-remote-shell-args ("-c"))
               (tramp-connection-timeout 10)
               (tramp-session-timeout 300)))

(tramp-set-completion-function "kubernetes" '((kubernetes-tramp--running-containers "")))

(provide 'kubernetes-el-tramp)
;;; kubernetes-el-tramp.el ends here
