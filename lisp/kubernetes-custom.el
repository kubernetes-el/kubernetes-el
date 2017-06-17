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

(defface kubernetes-context
  '((((class color) (background light)) :foreground "SkyBlue4")
    (((class color) (background  dark)) :foreground "LightSkyBlue1"))
  "Face for loading indicators."
  :group 'kubernetes)

(defface kubernetes-loading
  '((t (:inherit font-lock-comment-face)))
  "Face for loading indicators."
  :group 'kubernetes)

(defface kubernetes-namespace
  '((((class color) (background light)) :foreground "Goldenrod4")
    (((class color) (background  dark)) :foreground "LightGoldenrod2"))
  "Face for namespace references."
  :group 'kubernetes)

(provide 'kubernetes-custom)

;;; kubernetes-custom.el ends here
