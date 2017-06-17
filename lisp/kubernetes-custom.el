;;; kubernetes-custom.el --- Custom interface for kubernetes-el.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defgroup kubernetes nil
  "Emacs porcelain for Kubernetes."
  :group 'languages
  :prefix "kubernetes-")

(defcustom kubernetes-redraw-on-updates t
  "Whether to automatically redraw the overview when an update is received from the backend."
  :group 'kubernetes
  :type 'boolean)

(defface kubernetes-loading
  '((t (:inherit font-lock-comment-face)))
  "Face for loading indicators."
  :group 'kubernetes)

(provide 'kubernetes-custom)

;;; kubernetes-custom.el ends here
