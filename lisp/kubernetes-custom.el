;;; kubernetes-custom.el --- Custom interface for kubernetes-el.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'subr-x)

(defgroup kubernetes nil
  "Emacs porcelain for Kubernetes."
  :group 'languages
  :prefix "kubernetes-")


;; Custom variables

(defcustom kubernetes-kubectl-program "kubectl"
  "The path to kubectl."
  :group 'kubernetes-kubectl
  :type 'stringp)

(defun kubernetes-display-buffer-fullframe (buffer)
  (let* ((display-fn
          (lambda (buffer alist)
            (when-let (window (or (display-buffer-reuse-window buffer alist)
                                  (display-buffer-same-window buffer alist)
                                  (display-buffer-pop-up-window buffer alist)
                                  (display-buffer-use-some-window buffer alist)))
              (delete-other-windows window)
              window)))
         (window (display-buffer buffer (list display-fn))))
    (select-frame-set-input-focus
     (window-frame (select-window window)))))

(defcustom kubernetes-display-buffer-function #'kubernetes-display-buffer-fullframe
  "The function used display a Kubernetes buffer.

The function must take a single argument, which is the buffer to display."
  :group 'kubernetes
  :type '(radio (function-item kubernetes-display-buffer-fullframe)
                (function-item display-buffer)
                (function :tag "Function")))

(defun kubernetes-display-buffer (buf)
  (funcall kubernetes-display-buffer-function buf))

;; Faces

(defface kubernetes-context
  '((((class color) (background light)) :foreground "SkyBlue4")
    (((class color) (background  dark)) :foreground "LightSkyBlue1"))
  "Face for symbols referring to Kubernetes contexts."
  :group 'kubernetes)

(defface kubernetes-cluster
  '((t (:inherit default)))
  "Face for symbols referring to Kubernetes clusters."
  :group 'kubernetes)

(defface kubernetes-dimmed
  '((t (:inherit font-lock-comment-face)))
  "Face for loading indicators."
  :group 'kubernetes)

(defface kubernetes-loading
  '((t (:inherit kubernetes-dimmed)))
  "Face for loading indicators."
  :group 'kubernetes)

(defface kubernetes-namespace
  '((((class color) (background light)) :foreground "Goldenrod4")
    (((class color) (background  dark)) :foreground "LightGoldenrod2"))
  "Face for namespace references."
  :group 'kubernetes)


(provide 'kubernetes-custom)

;;; kubernetes-custom.el ends here
