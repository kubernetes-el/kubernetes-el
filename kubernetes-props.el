;;; kubernetes-props.el --- Functions used to decouple modules for testability.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defconst kubernetes-props
  '((message . message)
    (update-last-error . kubernetes-state-update-last-error)
    (overview-buffer-selected-p . kubernetes-utils-overview-buffer-selected-p)
    (get-last-error . (lambda ()
                        (kubernetes-state-last-error (kubernetes-state)))))
  "Variable used to inject functions across modules.")

(defun kubernetes-props-update-last-error (props message command time)
  (funcall (alist-get 'update-last-error props) message command time))

(defun kubernetes-props-get-last-error (props)
  (funcall (alist-get 'get-last-error props)))

(defun kubernetes-props-message (props fmt-string &rest args)
  (apply (alist-get 'message props) fmt-string args))

(defun kubernetes-props-overview-buffer-selected-p (props)
  (funcall (alist-get 'overview-buffer-selected-p props)))

(provide 'kubernetes-props)

;;; kubernetes-props.el ends here
