;;; kubernetes-errors.el --- Rendering for Kubernetes errors  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)

(defun kubernetes-errors-render (state)
  (-when-let* (((&alist 'error (&alist 'message message 'command command)) state)
               (header (concat (propertize "kubectl command failed" 'face 'font-lock-warning-face)))
               (message-paragraph
                (propertize (concat (with-temp-buffer
                                      (insert message)
                                      (fill-region (point-min) (point-max))
                                      (indent-region (point-min) (point-max) 2)
                                      (string-trim-right (buffer-string))))
                            'kubernetes-copy message))
               (command-str (string-join command " ")))

    `(section (error nil)
              (heading ,header)
              (padding)
              (section (message nil)
                       (line ,message-paragraph)
                       (padding))
              (section (command nil)
                       (copy-prop ,command-str (key-value 10 "Command" ,command-str))
                       (padding)))))



(provide 'kubernetes-errors)

;;; kubernetes-errors.el ends here
