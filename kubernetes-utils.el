;;; kubernetes-utils.el --- Common utilities.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 'subr-x)
(require 'term)

(require 'kubernetes-ast)
(require 'kubernetes-kubectl)
(require 'kubernetes-props)
(require 'kubernetes-state)
(require 'kubernetes-timers)
(require 'kubernetes-vars)

(autoload 'org-read-date "org")

(defun kubernetes-utils-read-pod-name (state)
  "Read a pod name from the user.

STATE is the current application state.

Update the pod state if it not set yet."
  (-let* (((&alist 'items pods)
           (or (kubernetes-state-pods state)
               (progn
                 (message "Getting pods...")
                 (let ((response (kubernetes-kubectl-await-on-async kubernetes-props state #'kubernetes-kubectl-get-pods)))
                   (kubernetes-state-update-pods response)
                   response))))
          (pods (append pods nil))
          (names (-map #'kubernetes-state-resource-name pods)))
    (completing-read "Pod: " names nil t)))

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
  "Return the names of all containers available in the specified pod."
  (-let [(&alist 'spec (&alist 'containers containers)) pod]
    (-map (-lambda ((&alist 'name name)) name) containers)))

(defun kubernetes-utils-read-container-name (&rest _)
  "Read a container name from the pod at POINT or a user-supplied pod."
  (letrec ((state (kubernetes-state))
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
  (let ((nav-buffer (get-buffer kubernetes-overview-buffer-name)))
    (with-current-buffer nav-buffer
      (pcase (get-text-property (point) 'kubernetes-nav nav-buffer)
        (`(:pod-name ,value)
         value)))))

(defun kubernetes-utils-ellipsize (s threshold)
  (if (> (length s) threshold)
      (concat (substring s 0 (1- threshold)) "…")
    s))

(defun kubernetes-utils-parse-utc-timestamp (timestamp)
  "Parse TIMESTAMP string from the API into the representation used by Emacs."
  (let ((parsed (parse-time-string (replace-regexp-in-string "Z" "" (replace-regexp-in-string "T" " " timestamp)))))
    (setf (nth 8 parsed) 0)
    parsed))

(defun kubernetes-utils-time-diff-string (start now)
  "Find the interval between START and NOW, and return a string of the coarsest unit."
  (let ((diff (time-to-seconds (time-subtract now start))))
    (car (split-string (format-seconds "%yy,%dd,%hh,%mm,%ss%z" diff) ","))))

(defun kubernetes-utils-kill-buffer (proc-buf &rest _)
  (if-let (win (get-buffer-window proc-buf))
      (quit-window t win)
    (kill-buffer proc-buf)))

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
        (kubernetes-timers-kill-timers)))))

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
           `((line ,(propertize time-str 'face 'magit-dimmed))
             (padding)
             (line ,(propertize command-str 'face 'magit-dimmed))
             (padding))))))

    (let ((proc (apply #'start-process "kubernetes-exec" buf command args)))
      (when process-filter
        (set-process-filter proc process-filter))
      (set-process-query-on-exit-flag proc nil))
    buf))

(defun kubernetes-utils-overview-buffer-selected-p ()
  (equal (current-buffer) (get-buffer kubernetes-overview-buffer-name)))

(defmacro kubernetes-utils--save-window-state (&rest body)
  "Restore window state after executing BODY.

This is useful if the buffer is erased and repopulated in BODY,
in which case `save-excursion' is insufficient to restore the
window state."
  `(let ((pos (point))
         (col (current-column))
         (window-start-line (window-start))
         (inhibit-redisplay t))
     (save-excursion
       ,@body)
     (goto-char pos)
     (move-to-column col)
     (set-window-start (selected-window) window-start-line)))

(defun kubernetes-utils-up-to-existing-dir (dir)
  (while (not (file-directory-p dir))
    (setq dir (file-name-directory (directory-file-name dir))))
  dir)

(provide 'kubernetes-utils)

;;; kubernetes-utils.el ends here
