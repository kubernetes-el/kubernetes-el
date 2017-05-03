;;; kubernetes-labels.el --- Utils for displaying resources with a particular label.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)

(require 'kubernetes-ast)
(require 'kubernetes-commands)
(require 'kubernetes-loading-container)
(require 'kubernetes-pods)
(require 'kubernetes-state)
(require 'kubernetes-utils)
(require 'kubernetes-vars)

;; Component

(kubernetes-ast-define-component labelled-pods-list (state)
  (-let* ((query (kubernetes-state-label-query state))
          ((&alist 'items pods) (kubernetes-state-pods state))
          (matches (nreverse (seq-reduce
                              (lambda (acc pod)
                                (if (equal query (kubernetes-state-resource-label pod))
                                    (cons `(pod ,state ,pod) acc)
                                  acc))
                              pods
                              nil))))
    `(section (root nil)
              (section (query-header nil)
                       (key-value 10 "Label" ,(propertize query 'face 'kubernetes-selector))
                       (padding))

              (indent
               (columnar-loading-container ,pods
                                           ,kubernetes-pods-column-heading
                                           ,matches)))))


;; Commands

(defun kubernetes-labels--redraw-buffer ()
  "Redraws the labels query buffer using the current state."
  (when-let (buf (get-buffer kubernetes-label-query-buffer-name))
    (with-current-buffer buf
      ;; Only redraw the buffer if it is in the selected window.
      (when (equal (get-buffer-window buf)
                   (selected-window))
        (kubernetes-utils--save-window-state
         (let ((inhibit-read-only t))
           (erase-buffer)
           (kubernetes-ast-eval `(labelled-pods-list ,(kubernetes-state)))))

        ;; Force the section at point to highlight.
        (magit-section-update-highlight)))))

(defun kubernetes-labels--initialize-buffer ()
  "Called the first time the labels buffer is opened to set up the buffer."
  (let ((buf (get-buffer-create kubernetes-label-query-buffer-name)))
    (with-current-buffer buf
      (kubernetes-mode)
      (kubernetes-timers-initialize-timers)
      (add-hook 'kubernetes-redraw-hook #'kubernetes-labels--redraw-buffer)
      (add-hook 'kill-buffer-hook (kubernetes-utils-make-cleanup-fn buf) nil t))
    buf))

;;;###autoload
(defun kubernetes-show-pods-for-label (label-query)
  "Display a buffer for pods matching a label.

LABEL-QUERY is a string used to match pods."
  (interactive
   (-let* (((&alist 'items pods) (kubernetes-state-pods (kubernetes-state)))
           (labels (-non-nil (-uniq (seq-map #'kubernetes-state-resource-label pods)))))
     (list (completing-read "Label: " labels))))

  (kubernetes-state-update-label-query label-query)
  (with-current-buffer (kubernetes-labels--initialize-buffer)
    (kubernetes-commands-display-buffer (current-buffer))
    (kubernetes-labels--redraw-buffer)))


(provide 'kubernetes-labels)

;;; kubernetes-labels.el ends here
