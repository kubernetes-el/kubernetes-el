;;; kubernetes.el --- Emacs porcelain for Kubernetes.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;; Version: 0.0.1

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

;;; Code:

(autoload 'json-read-from-string "json")

(defgroup kubernetes nil
  "Emacs porcelain for Kubernetes."
  :group 'tools
  :prefix "kubernetes-")

(defcustom kubernetes-kubectl-executable "kubectl"
  "The kubectl command used for Kubernetes commands."
  :group 'kubernetes
  :type 'string)

(defun kubernetes--kubectl (args on-success)
  "Run kubectl with ARGS.

ON-SUCCESS is a function of one argument, called with the process' buffer.

Returns the process object for this execution of kubectl."
  (let ((buf  (generate-new-buffer " kubectl")))
    (let ((process (apply #'start-process "kubectl" buf kubernetes-kubectl-executable args))
          (sentinel
           (lambda (_proc _status)
             (funcall on-success buf))))
      (set-process-sentinel process sentinel)
      process)))

;;;###autoload
(defun kubernetes-get-pods (cb)
  "Get all pods and execute callback CB with the parsed JSON."
  (kubernetes--kubectl '("get" "pods" "-o" "json")
             (lambda (buf)
               (let ((json (with-current-buffer buf
                             (json-read-from-string (buffer-string)))))
                 (funcall cb json)))))

(provide 'kubernetes)

;;; kubernetes.el ends here
