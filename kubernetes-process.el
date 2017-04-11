;;; kubernetes-process.el --- Process management.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'subr-x)
(require 'term)

(require 'kubernetes-ast)

(defun kubernetes-process-term-buffer-start (bufname command args)
  ;; Kill existing process.
  (when-let ((existing (get-buffer bufname))
             (proc (get-buffer-process existing)))
    (kubernetes-process-kill-quietly proc))

  (let ((buf (get-buffer-create bufname)))
    (with-current-buffer buf
      (erase-buffer)
      (buffer-disable-undo)
      (term-mode)
      (goto-char (point-min))
      (let ((time-str (format "Session started at %s" (substring (current-time-string) 0 19)))
            (command-str (format "%s %s" command (string-join args " "))))
        (kubernetes-ast-eval
         `((line ,(propertize time-str 'face 'magit-dimmed))
           (padding)
           (line ,(propertize command-str 'face 'magit-dimmed))
           (padding))))

      (term-exec (current-buffer) "kuberenetes-term" command nil args)
      (let ((proc (get-buffer-process (current-buffer))))
        (set-process-query-on-exit-flag proc nil)
        (term-char-mode)
        (add-hook 'kill-buffer-hook (lambda ()
                                      (when-let (win (get-buffer-window buf))
                                        (quit-window nil win)))
                  nil t)))

    buf))

(defun kubernetes-process-buffer-start (bufname setup-fn command args &optional process-filter)
  (let ((buf (get-buffer-create bufname)))
    (buffer-disable-undo buf)

    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (funcall setup-fn)
        (let ((time-str (format "Process started at %s" (substring (current-time-string) 0 19)))
              (command-str (format "%s %s" command (string-join args " "))))
          (kubernetes-ast-eval
           `((line ,(propertize time-str 'face 'magit-dimmed))
             (padding)
             (line ,(propertize command-str 'face 'magit-dimmed))
             (padding))))))

    (let ((proc (apply #'start-process "kubernetes-exec" buf command args)))
      (when process-filter
        (set-process-filter proc process-filter))
      (set-process-query-on-exit-flag proc nil))
    buf))

(defun kubernetes-process-kill-quietly (proc &optional _signal)
  (when proc
    (set-process-sentinel proc nil)
    (set-process-query-on-exit-flag proc nil)
    (let ((kill-buffer-query-functions nil)
          (buf (process-buffer proc)))
      (ignore-errors (kill-process proc))
      (ignore-errors (delete-process proc))
      (ignore-errors (kill-buffer buf)))))


;;; Background polling processes.

(defmacro kubernetes-state--define-polling-process (resource)
  "Create resource polling-related definitions.

RESOURCE is the name of the resource as a symbol.

Defines the following functions:

- `kubernetes-state-set-poll-RESOURCE-process'
- `kubernetes-state-release-poll-RESOURCE-process'
- `kubernetes-state-poll-RESOURCE-process'."
  (unless (symbolp resource) (error "RESOURCE must be a symbol"))
  (let ((proc-var-name (intern (format "kubernetes--internal-poll-%s-process" resource)))
        (proc-live-p (intern (format "kubernetes-state-poll-%s-process-live-p" resource)))
        (releaser-name (intern (format "kubernetes-state-release-poll-%s-process" resource)))
        (setter-name (intern (format "kubernetes-state-set-poll-%s-process" resource))))
    `(progn
       (defvar ,proc-var-name nil
         "Variable used to coordinate polling access to resources.

Do not use this variable directly. Instead, use its corresponding accessors.")

       (defun ,proc-live-p ()
         "Get the polling process for this resource if it is running."
         (when-let (proc ,proc-var-name)
           (when (process-live-p proc)
             proc)))

       (defun ,setter-name (proc)
         "Set the polling process to PROC."
         (,releaser-name)
         (setq ,proc-var-name proc))

       (defun ,releaser-name ()
         "Kill the existing polling process, if any."
         (kubernetes-process-kill-quietly ,proc-var-name)
         (setq ,proc-var-name nil)))))

(kubernetes-state--define-polling-process namespaces)
(kubernetes-state--define-polling-process context)
(kubernetes-state--define-polling-process pods)
(kubernetes-state--define-polling-process configmaps)
(kubernetes-state--define-polling-process secrets)
(kubernetes-state--define-polling-process services)

(defun kubernetes-state-kill-polling-processes ()
  (kubernetes-state-release-poll-namespaces-process)
  (kubernetes-state-release-poll-services-process)
  (kubernetes-state-release-poll-context-process)
  (kubernetes-state-release-poll-pods-process)
  (kubernetes-state-release-poll-configmaps-process)
  (kubernetes-state-release-poll-secrets-process))


(provide 'kubernetes-process)

;;; kubernetes-process.el ends here
