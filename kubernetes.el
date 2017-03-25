;;; kubernetes.el --- Emacs porcelain for Kubernetes.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;; Version: 0.0.1

;; Package-Requires: ((dash "2.12.1"))

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

(require 'dash)
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

(defcustom kubernetes-pod-restart-warning-threshold 5
  "The threshold for pod restarts above which a pod is highlighted."
  :group 'kubernetes
  :type 'number)

(defface kubernetes-context-name
  '((((class color) (background light)) :foreground "SkyBlue4")
    (((class color) (background  dark)) :foreground "LightSkyBlue1"))
  "Face for context names in report buffers."
  :group 'kubernetes)

(defface kubernetes-section-heading
  '((((class color) (background light)) :foreground "DarkGoldenrod4" :weight bold)
    (((class color) (background  dark)) :foreground "LightGoldenrod2" :weight bold))
  "Face for section headings."
  :group 'kubernetes)

(defface kubernetes-column-heading
  '((((class color) (background light)) :foreground "grey30" :weight bold)
    (((class color) (background  dark)) :foreground "grey80" :weight bold))
  "Face for section headings."
  :group 'kubernetes)

(defface kubernetes-progress-indicator
  '((t :inherit shadow))
  "Face for progress indicators."
  :group 'kubernetes)

(defface kubernetes-dimmed
  '((t :inherit shadow))
  "Face for things that shouldn't stand out."
  :group 'kubernetes)

(defconst kubernetes-display-pods-buffer-name "*kubernetes pods*")

(defconst kubernetes-display-context-buffer-name "*kubernetes context*")


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

(defun kubernetes-make-set-heading-cb (marker &optional update-line-fn)
  (unless update-line-fn
    (setq update-line-fn #'insert))
  (lambda (response)
    (with-current-buffer (marker-buffer marker)
      (save-excursion
        (goto-char (marker-position marker))
        (let ((inhibit-read-only t))
          (delete-region (point) (line-end-position))
          (funcall update-line-fn response))))))

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



;;; Displaying all pods

(defun kubernetes--insert-context-section ()
  (insert (format "%-12s" "Context: "))
  (let ((marker (make-marker))
        (update-line (lambda (config)
                       (-let [(&alist 'current-context current 'contexts contexts) config]
                         (insert (propertize (concat (or current "<none>") "\n") 'face 'kubernetes-context-name))
                         (-when-let* ((ctx (--find (equal current (alist-get 'name it)) (append contexts nil)))
                                      ((&alist 'name n 'context (&alist 'cluster c 'namespace ns)) ctx))
                           (unless (string-empty-p c)
                             (insert (format "%-12s%s\n" "Cluster: " c)))
                           (unless (string-empty-p ns)
                             (insert (format "%-12s%s" "Namespace: " ns))))))))
    (set-marker marker (point))
    (kubernetes-config-view
     (kubernetes-make-set-heading-cb marker update-line)))

  (newline))

(defun kubernetes--ellispsize (s threshold)
  (if (> (length s) threshold)
      (concat (substring s 0 (1- threshold)) "…")
    s))

(defun kubernetes--format-pod-line (pod)
  (-let* (((&alist 'metadata (&alist 'name name)
                   'status (&alist 'containerStatuses [(&alist 'restartCount restarts)]
                                   'phase phase))
           pod)
          (str
           (concat (format "%-40s " (kubernetes--ellispsize name 40))
                   (let ((s (format "%-17s " phase)))
                     (if (equal phase "Running") (propertize s 'face 'kubernetes-dimmed) s))
                   (let ((s (format "%s" restarts)))
                     (cond
                      ((equal 0 restarts)
                       (propertize s 'face 'kubernetes-dimmed))
                      ((<= kubernetes-pod-restart-warning-threshold restarts)
                       (propertize s 'face 'warning))
                      (t
                       s)))))
          (str (cond
                ((member (downcase phase) '("running" "containercreating" "terminated"))
                 str)
                ((member (downcase phase) '("runcontainererror" "crashloopbackoff"))
                 (propertize str 'face 'error))
                (t
                 (propertize str 'face 'warning))))
          (str (concat "  " str)))
    (propertize str 'kubernetes-nav (list :pod pod))))

(defun kubernetes--insert-pods-section ()
  (insert (propertize "Pods " 'face 'kubernetes-section-heading))
  (let ((count-marker (make-marker))
        (pods-marker (make-marker)))
    (set-marker count-marker (point))
    (newline)
    (insert (propertize (format "  %-40s %-17s %s\n" "Name" "Status" "Restarts")
                        'face 'kubernetes-column-heading))
    (set-marker pods-marker (point))
    (insert (propertize "  Fetching..." 'face 'kubernetes-progress-indicator))
    (kubernetes-get-pods
     (lambda (response)
       (-let [(&alist 'items pods) response]
         (with-current-buffer (marker-buffer count-marker)
           (save-excursion
             (goto-char (marker-position count-marker))
             (let ((inhibit-read-only t))
               (delete-region (point) (line-end-position))
               (insert (format "(%s)" (length pods)))))
           (save-excursion
             (goto-char (marker-position pods-marker))
             (let ((inhibit-read-only t))
               (delete-region (point) (line-end-position))
               (--each (append pods nil)
                 (insert (kubernetes--format-pod-line it))
                 (newline))))))))))

;;;###autoload
(defun kubernetes-display-pods-refresh ()
  "Create or refresh the Kubernetes pods buffer."
  (interactive)
  (let ((buf (get-buffer-create kubernetes-display-pods-buffer-name)))
    (with-current-buffer buf
      (kubernetes-display-pods-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (kubernetes--insert-context-section)
        (newline)
        (kubernetes--insert-pods-section)))
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
