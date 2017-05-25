;;; kubernetes-overview.el --- Main overview buffer.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'kubernetes-mode)
(require 'kubernetes-pods-list)
(require 'kubernetes-props)

(defconst kubernetes-overview-props
  '((start-client . kubernetes-client-start)
    (stop-client . kubernetes-client-stop)
    (get-client-process . kubernetes-state-client-process)
    (get-state . kubernetes-state)
    (clear-state . kubernetes-state-clear)
    (ast-eval . kubernetes-ast-eval)
    (get-namespace . kubernetes-state-namespace)
    (set-namespace . kubernetes-state-set-namespace)
    (display-buffer . display-buffer))
  "Functions to inject for isolation and testing.")

(defvar kubernetes-overview-buffer "*kubernetes-overview*")


(defun kubernetes-overview--redraw (buffer props)
  (kubernetes-props-bind ([ast-eval get-state] props)
    (with-current-buffer buffer
      (read-only-mode +1)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (ast-eval `(section (root nil)
                            (pods-list ,(get-state))))))))

(defun kubernetes-overview (props namespace)
  "Show the overview buffer.

PROPS is an alist of functions to inject.

NAMESPACE is the namespace to use."
  (interactive (list kubernetes-overview-props
                     (kubernetes-props-bind ([get-namespace] kubernetes-overview-props)
                       (read-string "Namespace: " (get-namespace) 'kubernetes-namespace))))
  (kubernetes-props-bind ([clear-state set-namespace get-namespace start-client stop-client get-client-process display-buffer] props)
    (cond
     ;; Restart the process if the configuration has changed.
     ((and (get-client-process)
           (not (equal (get-namespace) namespace)))
      (clear-state)
      (set-namespace namespace)
      (stop-client)
      (start-client))

     ;; Process is running with current configuration.
     ((get-client-process))

     (t
      (set-namespace namespace)
      (start-client)))

    (let ((buffer (get-buffer-create kubernetes-overview-buffer)))
      ;; Stop the polling process if the overview buffer is deleted.
      (with-current-buffer buffer
        (kubernetes-mode)
        (add-hook 'kill-buffer-hook (lambda ()
                                      (stop-client)
                                      (clear-state))
                  nil t))

      ;; Redraw buffer whenever the client state is updated.
      (add-hook 'kubernetes-state-client-message-processed-functions
                (lambda (_)
                  (when (buffer-live-p buffer)
                    (kubernetes-overview--redraw buffer props))))

      (kubernetes-overview--redraw buffer props)
      (display-buffer buffer))))


(provide 'kubernetes-overview)

;;; kubernetes-overview.el ends here
