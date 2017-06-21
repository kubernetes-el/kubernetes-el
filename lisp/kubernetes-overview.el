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
    (ast-render . kubernetes-ast-render)
    (get-namespace . kubernetes-state-namespace)
    (set-namespace . kubernetes-state-set-namespace)
    (region-active-p . region-active-p)
    (get-buffer . get-buffer)
    (buffer-live-p . buffer-live-p)
    (kubernetes-overview-redraw . kubernetes-overview-redraw))
  "Functions to inject for isolation and testing.")

(defvar kubernetes-overview-buffer "*kubernetes-overview*")


;; Components

(kubernetes-ast-define-component errors (state)
  (-when-let ((&alist 'msg msg 'error err) (kubernetes-state-error state))
    `(section (errors)
              (heading (propertize (face error) "Error"))
              (indent
               ,msg
               (key-value 12 "Reason" ,err))
              (padding))))

(kubernetes-ast-define-component overview (state)
  `(section (root nil)
            (errors ,state)
            (config ,state)
            (pods-list ,state)))


;; Lifecycle and drawing

(defun kubernetes-overview-redraw (buffer props)
  "Redraw the overview buffer."
  (interactive (list (get-buffer kubernetes-overview-buffer) kubernetes-overview-props))
  (unless buffer
    (user-error "Overview buffer not active"))
  (kubernetes-props-bind ([ast-render get-state region-active-p] props)
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        ;; If a region is active, a redraw would affect the region in
        ;; unpredictable ways.
        (unless (region-active-p)
          (ast-render buffer `(overview ,(get-state))))))))

(defun kubernetes-overview--handle-client-message (_msg &optional props)
  (let ((props (or props kubernetes-overview-props)))
    (kubernetes-props-bind ([get-buffer buffer-live-p kubernetes-overview-redraw] props)
      (when-let (buf (get-buffer kubernetes-overview-buffer))
        (when (buffer-live-p buf)
          (kubernetes-overview-redraw buf props))))))

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

(defun kubernetes-overview--tear-down-overview (&optional props)
  (kubernetes-props-bind ([clear-state stop-client] (or props kubernetes-overview-props))
    (stop-client)
    (clear-state)))

(defun kubernetes-overview--setup-buffer (props)
  (kubernetes-props-bind ([kubernetes-overview-redraw] props)
    (let ((buffer (get-buffer-create kubernetes-overview-buffer)))
      (with-current-buffer buffer
        (kubernetes-mode)
        (add-hook 'kill-buffer-hook #'kubernetes-overview--tear-down-overview nil t))

      ;; Redraw buffer whenever the client state is updated.
      (add-hook 'kubernetes-state-should-redraw-functions
                #'kubernetes-overview--handle-client-message)

      (kubernetes-overview-redraw buffer props)
      buffer)))


;; Main entrypoint

(defun kubernetes-overview (props namespace)
  "Show the overview buffer.

PROPS is an alist of functions to inject.

NAMESPACE is the namespace to use."
       (interactive
        (list kubernetes-overview-props
              (or (kubernetes-state-namespace)
                  (kubernetes-kubectl-current-namespace)
                  (kubernetes-kubectl-read-namespace))))
       (kubernetes-overview--initialize-client props namespace)
       (let ((buffer (or (get-buffer kubernetes-overview-buffer) (kubernetes-overview--setup-buffer props))))
         (kubernetes-display-buffer buffer)))

(provide 'kubernetes-overview)

;;; kubernetes-overview.el ends here
