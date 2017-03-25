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

(require 'subr-x)
(autoload 'json-read-from-string "json")

(defgroup kubernetes nil
  "Emacs porcelain for Kubernetes."
  :group 'tools
  :prefix "kubernetes-")

(defcustom kubernetes-kubectl-executable "kubectl"
  "The kubectl command used for Kubernetes commands."
  :group 'kubernetes
  :type 'string)

(defface kubernetes-context-name
  '((((class color) (background light)) :foreground "SkyBlue4")
    (((class color) (background  dark)) :foreground "LightSkyBlue1"))
  "Face for context names in report buffers."
  :group 'kubernetes)

(defcustom kubernetes-display-buffer-select t
  "Whether to select Kubernetes buffers automatically."
  :group 'kubernetes
  :type 'boolean)

(defcustom kubernetes-display-buffer-function #'kubernetes-display-buffer-fullframe
  "The function used display a Kubernetes buffer.

The function must take a single argument, which is the buffer to display."
  :group 'kubernetes
  :type '(radio (function-item kubernetes-display-buffer-fullframe)
                (function-item display-buffer)
                (function :tag "Function")))

(defconst kubernetes-display-pods-buffer-name "*kubernetes pods*")


;; Main Kubernetes query routines

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

;;;###autoload
(defun kubernetes-config-view (cb)
  "Get the current configuration and pass it to CB."
  (kubernetes--kubectl '("config" "view" "-o" "json")
             (lambda (buf)
               (let ((json (with-current-buffer buf
                             (json-read-from-string (buffer-string)))))
                 (funcall cb json)))))

(defun kubernetes--await-on-async (fn)
  "Turn an async function requiring a callback into a synchronous one.

Transforms a function of type:

  FN : (a -> b) -> process

to a function of the type:

  FN' : () -> a"
  (let* (complete result)
    (funcall fn (lambda (response)
                  (setq complete t)
                  (setq result response)))

    (while (not complete)
      (sleep-for 0.001))

    result))

;; View management

(defun kubernetes-make-set-heading-cb (marker &optional value-format-fn)
  (unless value-format-fn
    (setq value-format-fn #'identity))
  (lambda (response)
    (with-current-buffer (marker-buffer marker)
      (save-excursion
        (goto-char (marker-position marker))
        (let ((inhibit-read-only t))
          (delete-region (point) (line-end-position))
          (insert (funcall value-format-fn response)))))))

(defun kubernetes-display-buffer-fullframe (buffer)
  (let ((display-fn
         (lambda (buffer alist)
           (when-let (window (or (display-buffer-reuse-window buffer alist)
                                 (display-buffer-same-window buffer alist)
                                 (display-buffer-pop-up-window buffer alist)
                                 (display-buffer-use-some-window buffer alist)))
             (delete-other-windows window)
             window))))
    (display-buffer buffer (list display-fn))))

(defun kubernetes-display-buffer (buffer)
  (let ((window (funcall kubernetes-display-buffer-function buffer)))
    (when kubernetes-display-buffer-select
      (select-frame-set-input-focus
       (window-frame (select-window window))))))

(defun kubernetes-visit-thing (pos)
  "Visit the thing at POS.

Inspects the text properties of the object at point to determine
how to transition to the next view."
  (interactive "d")
  (pcase (get-text-property pos 'kubernetes-nav)
    (`(:context ,ctx)
     (kubernetes-display-context ctx))))


;;; Displaying pods

;;;###autoload
(defun kubernetes-display-pods-refresh ()
  "Create or refresh the Kubernetes pods buffer."
  (interactive)
  (let ((buf (get-buffer-create kubernetes-display-pods-buffer-name)))
    (with-current-buffer buf
      (kubernetes-display-pods-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "%-10s" "Context: "))
        (let ((marker (make-marker))
              (format-context (lambda (config)
                                (let ((ctx (alist-get 'current-context config "<none>")))
                                  (propertize ctx 'face 'kubernetes-context-name)))))
          (set-marker marker (point))
          (kubernetes-config-view
           (kubernetes-make-set-heading-cb marker format-context apply-nav-property)))))
    buf))

(defvar kubernetes-display-pods-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "g") #'kubernetes-display-pods-refresh)
    (define-key keymap (kbd "q") #'quit-window)
    keymap)
  "Keymap for `kubernetes-display-pods-mode'.")

(define-derived-mode kubernetes-display-pods-mode special-mode "Kubernetes Pods"
  "Mode for working with Kubernetes pods.

\\<kubernetes-display-pods-mode-map>\
Type \\[kubernetes-display-pods-refresh] to refresh the buffer.

\\{kubernetes-display-pods-mode-map}"
  :group 'kubernetes
  (read-only-mode +1))

;;;###autoload
(defun kubernetes-display-pods ()
  "Display a list of pods in the current Kubernetes context."
  (interactive)
  (with-current-buffer (kubernetes-display-pods-refresh)
    (goto-char (point-min))
    (kubernetes-display-buffer (current-buffer))))


(provide 'kubernetes)

;;; kubernetes.el ends here
