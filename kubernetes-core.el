;; kubernetes-core.el --- core functionality -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'kubernetes-vars)

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

(provide 'kubernetes-core)
;;; kubernetes-core.el ends here
