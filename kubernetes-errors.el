;;; kubernetes-errors.el --- Rendering for Kubernetes errors  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 'subr-x)

(require 'kubernetes-state)

(defun kubernetes-errors-render (state)
  (-when-let* (((&alist 'message message 'command command) (kubernetes-state-last-error state))
               (message-paragraph
                (with-temp-buffer
                  (insert message)
                  (fill-region (point-min) (point-max))
                  (indent-region (point-min) (point-max) 2)
                  (string-trim-right (buffer-string)))))

    `(section (error nil)
              (heading (propertize (face font-lock-warning-face) "kubectl command failed"))
              (padding)
              (section (message nil)
                       (copy-prop ,message (line ,message-paragraph))
                       (padding))
              (section (command nil)
                       (key-value 10 "Command" ,command)
                       (padding)))))

(provide 'kubernetes-errors)

;;; kubernetes-errors.el ends here
