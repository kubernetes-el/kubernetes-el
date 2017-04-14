;;; kubernetes-namespaces.el --- Helpers for Kubernetes namespaces.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'kubernetes-kubectl)
(require 'kubernetes-process)
(require 'kubernetes-state)
(require 'kubernetes-vars)

(defun kubernetes-namespaces-refresh (&optional interactive)
  (unless (kubernetes-process-poll-namespaces-process-live-p)
    (kubernetes-process-set-poll-namespaces-process
     (kubernetes-kubectl-get-namespaces kubernetes-default-props
                                        (kubernetes-state)
                                        (lambda (response)
                                          (kubernetes-state-update-namespaces response)
                                          (when interactive
                                            (message "Updated namespaces.")))
                                        (lambda ()
                                          (kubernetes-process-release-poll-namespaces-process))))))


(provide 'kubernetes-namespaces)

;;; kubernetes-namespaces.el ends here
