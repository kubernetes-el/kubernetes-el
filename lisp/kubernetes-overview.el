;;; kubernetes-overview.el --- Main overview buffer.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'kubernetes-ast)
(require 'kubernetes-custom)
(require 'kubernetes-mode)
(require 'kubernetes-pods-list)
(require 'kubernetes-props)

(defconst kubernetes-overview-props
  '((start-client . kubernetes-client-start)
    (stop-client . kubernetes-client-stop)
    (get-client-process . kubernetes-state-client-process)
    (get-state . kubernetes-state)
    (reset-state . kubernetes-state-reset)
    (clear-state . kubernetes-state-clear)
    (ast-eval . kubernetes-ast-eval)
    (get-namespace . kubernetes-state-namespace)
    (set-namespace . kubernetes-state-set-namespace)
    (updates-received-p . kubernetes-state-updates-received-p)
    (display-buffer . display-buffer)
    (buffer-live-p . buffer-live-p)
    (kubernetes-overview-redraw . kubernetes-overview-redraw))
  "Functions to inject for isolation and testing.")

(defvar kubernetes-overview-buffer "*kubernetes-overview*")

(defun kubernetes-overview-redraw (buffer props)
  "Redraw the overview buffer."
  (interactive (list (get-buffer kubernetes-overview-buffer) kubernetes-overview-props))
  (unless buffer
    (user-error "Overview buffer not active"))
  (kubernetes-props-bind ([ast-eval get-state] props)
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        ;; If a region is active, a redraw would affect the region in
        ;; unpredictable ways.
        (unless (region-active-p)
          (let ((state (get-state)))
            (kubernetes-ast-render buffer
                                   `(section (root nil)
                                             (config ,state)
                                             (pods-list ,state)))))))))

(defun kubernetes-overview--mk-client-message-handler (props overview-buffer)
  (kubernetes-props-bind ([updates-received-p buffer-live-p kubernetes-overview-redraw] props)
    (lambda (_msg)
      (when (and (buffer-live-p overview-buffer)
                 (or kubernetes-redraw-on-updates
                     (not (updates-received-p))))
        (kubernetes-overview-redraw overview-buffer props)
        t))))

(defun kubernetes-overview--initialize-client (props namespace)
  (kubernetes-props-bind ([reset-state
                           set-namespace get-namespace
                           start-client stop-client
                           get-client-process] props)
    (cond
     ;; Restart the process if the configuration has changed.
     ((and (get-client-process)
           (not (equal (get-namespace) namespace)))
      (reset-state)
      (set-namespace namespace)
      (stop-client)
      (start-client))

     ;; Process already running with current configuration.
     ((get-client-process))

     (t
      (reset-state)
      (set-namespace namespace)
      (start-client)))))

(defun kubernetes-overview (props namespace)
  "Show the overview buffer.

PROPS is an alist of functions to inject.

NAMESPACE is the namespace to use."
       (interactive
        (progn
          (kubernetes-state-marshal-from-kubectl)
          (list kubernetes-overview-props
                (or (kubernetes-state-namespace) (read-string "Namespace: " nil 'kubernetes-namespace)))))
       (kubernetes-props-bind ([clear-state stop-client display-buffer] props)
         (kubernetes-overview--initialize-client props namespace)

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
                     (kubernetes-overview--mk-client-message-handler props buffer))

           (kubernetes-overview-redraw buffer props)
           (display-buffer buffer))))

(provide 'kubernetes-overview)

;;; kubernetes-overview.el ends here
