;;; kubernetes-timers.el --- Internal timers.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'subr-x)
(require 'kubernetes-vars)

(defvar kubernetes-timers--poll-timer nil
  "Background timer used to poll for updates.

This is used to regularly synchronise local state with Kubernetes.")

(defvar kubernetes-timers--redraw-timer nil
  "Background timer used to trigger buffer redrawing.

This is used to display the current state.")

(defun kubernetes-timers-initialize-timers ()
  (unless kubernetes-timers--redraw-timer
    (setq kubernetes-timers--redraw-timer (run-with-timer kubernetes-redraw-frequency kubernetes-redraw-frequency 'kubernetes--redraw-buffers)))
  (unless kubernetes-timers--poll-timer
    (setq kubernetes-timers--poll-timer (run-with-timer kubernetes-poll-frequency kubernetes-poll-frequency 'kubernetes-refresh))))

(defun kubernetes-timers-kill-timers ()
  (when-let (timer kubernetes-timers--redraw-timer)
    (cancel-timer timer))
  (when-let (timer kubernetes-timers--poll-timer)
    (cancel-timer timer))
  (setq kubernetes-timers--redraw-timer nil)
  (setq kubernetes-timers--poll-timer nil))


(provide 'kubernetes-timers)

;;; kubernetes-timers.el ends here
