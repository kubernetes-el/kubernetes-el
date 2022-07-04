;; kubernetes-core.el --- core functionality -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'magit-section)
(require 's)

(require 'kubernetes-ast)
(require 'kubernetes-vars)


(defun kubernetes--parse-utc-timestamp (timestamp)
  "Parse TIMESTAMP string from the API into the representation used by Emacs."
  (let ((parsed (parse-time-string (replace-regexp-in-string "Z" "" (replace-regexp-in-string "T" " " timestamp)))))
    (setf (nth 8 parsed) 0)
    parsed))

(defvar kubernetes-state--current-state nil)

(defmacro kubernetes--save-window-state (&rest body)
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


(defun kubernetes-state ()
  kubernetes-state--current-state)

(defun kubernetes--overview-render (state)
  (let ((sections (kubernetes-state-overview-sections state)))
    `(section (root nil)
              ,(kubernetes-errors-render state)
              ,(when (member 'context sections)
                 (kubernetes-contexts-render state))
              ,(mapcar (lambda (section)
                         `(,(intern (if (eq section 'overview)
                                        "aggregated-view"
                                      (format "%s-list" section)))
                           ,state))
                       (remove 'context sections)))))

(defun kubernetes--redraw-overview-buffer ()
  "Redraws the main buffer using the current state."
  (when-let (buf (get-buffer kubernetes-overview-buffer-name))
    (with-current-buffer buf
      ;; If a region is active, a redraw would affect the region in
      ;; unpredictable ways.
      (unless (region-active-p)
        ;; Suppress redrawing if the overview is not selected. This prevents
        ;; point from jumping around when a magit popup is open.
        (when (member (selected-window) (get-buffer-window-list buf))
          (kubernetes--save-window-state
           (let ((inhibit-read-only t))
             (erase-buffer)
             (kubernetes-ast-eval (kubernetes--overview-render (kubernetes-state)))))

          ;; Force the section at point to highlight.
          (magit-section-update-highlight))))))

(defun kubernetes--message (format &rest args)
  "Call `message' with FORMAT and ARGS.

We `inhibit-message' the message when the cursor is in the
minibuffer and when Emacs version is before Emacs 27 due to the
fact that we often use `kubernetes--info', `kubernetes--warn' and
`kubernetes--error' in async context and the call to these
function is removing the minibuffer prompt.  The issue with async
messages is already fixed in Emacs 27."
  (when kubernetes-show-message
    (let ((inhibit-message (and (minibufferp)
                                (version< emacs-version "27.0"))))
      (apply #'message format args))))

(defun kubernetes--info (format &rest args)
  "Display kubernetes info message with FORMAT with ARGS."
  (kubernetes--message "%s :: %s" (propertize "k8s" 'face 'success) (apply #'format format args)))

(defun kubernetes--warn (format &rest args)
  "Display kubernetes warn message with FORMAT with ARGS."
  (kubernetes--message "%s :: %s" (propertize "k8s" 'face 'warning) (apply #'format format args)))

(defun kubernetes--error (format &rest args)
  "Display kubernetes error message with FORMAT with ARGS."
  (kubernetes--message "%s :: %s" (propertize "k8s" 'face 'error) (apply #'format format args)))

(defun kubernetes--val-from-arg-list (arg-list key)
  "Find value for flag KEY in CLI-flag-style ARG-LIST.
Flag-value pairs in ARG-LIST can be either separate or paired with =,
  e.g. '(\"--foo\" bar) or '(\"--foo=bar\").
This function expects long flags only.
If ARG-LIST is nil or KEY is not present in ARG-LIST, returns nil."
  (when arg-list
    (-when-let* ((key-index (--find-index
                             (s-prefix? (format "--%s" (symbol-name key)) it)
                             arg-list))
                 (key-val (nth key-index arg-list)))
      (if (s-contains? "=" key-val)
          (cadr (s-split "=" key-val))
        (nth (+ 1 key-index) arg-list)))))

(defvar kubernetes--poll-timer nil
"Background timer used to poll for updates.

This is used to regularly synchronise local state with Kubernetes.")

(defvar kubernetes--redraw-timer nil
  "Background timer used to trigger buffer redrawing.

This is used to display the current state.")

(defun kubernetes--initialize-timers ()
  "Initialize kubernetes.el global timers.

Global timers are responsible for overview redrawing and resource
polling according to `kubernetes-redraw-frequency' and
`kubernetes-poll-frequency', respectively."
  (unless kubernetes--redraw-timer
    (setq kubernetes--redraw-timer (run-with-timer 0 kubernetes-redraw-frequency #'kubernetes-state-trigger-redraw)))
  (unless kubernetes--poll-timer
    (setq kubernetes--poll-timer (run-with-timer 0 kubernetes-poll-frequency
                                       (lambda ()
                                         (run-hooks 'kubernetes-poll-hook))))))

(defun kubernetes--kill-timers ()
  "Kill kubernetes.el global timers."
  (when-let (timer kubernetes--redraw-timer)
    (cancel-timer timer))
  (when-let (timer kubernetes--poll-timer)
    (cancel-timer timer))
  (setq kubernetes--redraw-timer nil)
  (setq kubernetes--poll-timer nil))

(defun kubernetes--time-diff-string (start now)
  "Find the interval between START and NOW, and return a string of the coarsest unit."
  (let ((diff (time-to-seconds (time-subtract now start))))
    (car (split-string (format-seconds "%yy,%dd,%hh,%mm,%ss%z" diff) ","))))

(defun kubernetes--overview-buffer-selected-p ()
  (equal (current-buffer) (get-buffer kubernetes-overview-buffer-name)))

(provide 'kubernetes-core)
;;; kubernetes-core.el ends here
