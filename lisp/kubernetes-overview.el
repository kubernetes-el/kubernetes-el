;;; kubernetes-overview.el --- Main overview buffer.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'kubernetes-props)
(require 'kubernetes-pods-list)

(defconst kubernetes-overview-props
  '((client-start . kubernetes-client-start)
    (client-process . kubernetes-state-client-process)
    (state . kubernetes-state)
    (ast-eval . kubernetes-ast-eval)
    (set-namespace . kubernetes-state-set-namespace)
    (display-buffer . display-buffer))
  "Functions to inject for isolation and testing.")

(defvar kubernetes-overview-buffer "*kubernetes-overview*")


(defun kubernetes-overview--redraw (buffer props)
  (kubernetes-props-bind ([ast-eval state] props)
    (with-current-buffer buffer
      (read-only-mode +1)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (funcall ast-eval `(section (root nil)
                                    (pods-list ,(funcall state))))))))

(defun kubernetes-overview (props namespace)
  "Show the overview buffer.

PROPS is an alist of functions to inject.

NAMESPACE is the namespace to use."
  (interactive (list kubernetes-overview-props
                     (read-string "Namespace: " nil 'kubernetes-namespace)))
  (kubernetes-props-bind ([set-namespace client-process client-start display-buffer] props)
    (unless (funcall client-process)
      (funcall set-namespace namespace)
      (funcall client-start))

    (let ((buffer (get-buffer-create kubernetes-overview-buffer)))
      ;; Redraw buffer whenever the client state is updated.
      (add-hook 'kubernetes-state-client-message-processed-functions
                (lambda (_)
                  (when (buffer-live-p buffer)
                    (kubernetes-overview--redraw buffer props))))

      (kubernetes-overview--redraw buffer props)
      (funcall display-buffer buffer))))


(provide 'kubernetes-overview)

;;; kubernetes-overview.el ends here
