;; kubernetes-utils.el --- Common utilities.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 'subr-x)
(require 'term)

(require 'kubernetes-ast)
(require 'kubernetes-core)
(require 'kubernetes-kubectl)
(require 'kubernetes-state)
(require 'kubernetes-vars)

(autoload 'org-read-date "org")

(defun kubernetes-utils-read-iso-datetime (&rest _)
  (let* ((date (org-read-date nil t))
         (tz (format-time-string "%z" date)))
    (concat
     (format-time-string "%Y-%m-%dT%H:%M:%S" date)
     (replace-regexp-in-string (rx (group (? (any "+-")) digit digit)
                                   (group digit digit))
                               "\\1:\\2"
                               tz))))

(defun kubernetes-get-pod-container-names (pod)
  "Return the names of all containers available in the specified POD.

Returns nil on invalid input."
  (-let [(&alist 'spec (&alist 'containers containers)) pod]
    (-map (-lambda ((&alist 'name name)) name) containers)))

(define-error 'kubernetes-state-error "Kubernetes state not initialized")

(defun kubernetes-utils-read-container-name (&rest _)
  "Read a container name from the pod at POINT or a user-supplied pod.

This function will error if `kubernetes-state' is not
initialized."
  (letrec ((state (or (kubernetes-state) (signal 'kubernetes-state-error nil)))
           (pod-name (or (kubernetes-utils-maybe-pod-name-at-point)
                         (kubernetes-utils-read-pod-name state)))
           (pod (kubernetes-state-lookup-pod pod-name state))
           (pod-containers (kubernetes-get-pod-container-names pod)))
    (completing-read "Container name: " pod-containers nil t)))

(defun kubernetes-utils-read-time-value (&rest _)
  "Read a relative time value in the style accepted by kubectl.  E.g. 20s, 3h, 5m."
  (let (result)
    (while (null result)
      (let ((input (read-string "Time value (e.g. 20s): ")))
        (if (string-match-p (rx bol (* space) (+ digit) (* space) (any "smh") (* space) eol)
                            input)
            (setq result input)
          (message "Invalid time value")
          (sit-for 1))))
    result))

(defun kubernetes-utils-maybe-pod-name-at-point ()
  (when-let ((nav-buffer (get-buffer kubernetes-overview-buffer-name)))
    (with-current-buffer nav-buffer
      (pcase (get-text-property (point) 'kubernetes-nav nav-buffer)
        (`(:pod-name ,value)
         value)))))

(defalias 'kubernetes-utils-parse-utc-timestamp 'kubernetes--parse-utc-timestamp)

(defun kubernetes-utils-make-cleanup-fn (buf)
  "Make a function to add to `kill-buffer-hook' for a Kubernetes buffer.

BUF is the buffer used to display a Kubernetes feature.  A
reference to it is needed to determine which buffers remain.

The function will terminate polling when the last Kubernetes
buffer is killed."
  (lambda ()
    (let* ((bufs (-keep #'get-buffer (list kubernetes-label-query-buffer-name
                                           kubernetes-overview-buffer-name)))
           (more-buffers (remove buf bufs)))
      (unless more-buffers
        (dolist (b bufs)
          (with-current-buffer b
            (kubernetes-state-clear)))
        (kubernetes-process-kill-polling-processes)
        (kubernetes--kill-timers)))))

(defun kubernetes-utils-term-buffer-start (bufname command args)
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
         `((line ,(propertize time-str 'face 'kubernetes-dimmed))
           (padding)
           (line ,(propertize command-str 'face 'kubernetes-dimmed))
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

(defun kubernetes-utils-vterm-start (bufname command args)
  ;; Kill existing process.
  (when-let ((existing (get-buffer bufname)))
    (let ((proc (get-buffer-process existing)))
      (if proc
          (kubernetes-process-kill-quietly proc)
        (kill-buffer bufname))))

  (let* ((vterm-buffer-name bufname)
         (command-str (format "%s %s" command (string-join args " ")))
         (vterm-shell command-str))
    (vterm-other-window)))

(defun kubernetes-utils-process-buffer-start (bufname setup-fn command args &optional process-filter)
  (let ((buf (get-buffer-create bufname)))
    (buffer-disable-undo buf)

    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (funcall setup-fn)
        (let ((time-str (format "Process started at %s" (substring (current-time-string) 0 19)))
              (command-str (format "%s %s" command (string-join args " "))))
          (kubernetes-ast-eval
           `((line ,(propertize time-str 'face 'kubernetes-dimmed))
             (padding)
             (line ,(propertize command-str 'face 'kubernetes-dimmed))
             (padding))))))

    (let ((proc (apply #'start-process "kubernetes-exec" buf command args)))
      (when process-filter
        (set-process-filter proc process-filter))
      (set-process-query-on-exit-flag proc nil))
    buf))

(defun kubernetes-utils-up-to-existing-dir (dir)
  (while (not (file-directory-p dir))
    (setq dir (file-name-directory (directory-file-name dir))))
  dir)

(provide 'kubernetes-utils)

;;; kubernetes-utils.el ends here
