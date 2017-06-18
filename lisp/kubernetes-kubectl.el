;;; kubernetes-kubectl.el --- Integration with kubectl.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'kubernetes-custom)

(require 'dash)
(require 'json)
(require 'subr-x)

(defun kubernetes-kubectl-kubeconfig-settings ()
  "Return an alist of settings for the active Kubernetes context."
  (let* ((lines (process-lines kubernetes-kubectl-program "config" "view" "-o" "json"))
         (config (json-read-from-string (string-join lines "\n"))))

    (-when-let* (((&alist 'current-context context
                          'contexts contexts) config)
                 (context-settings
                  (alist-get 'context
                             (--find (equal (alist-get 'name it) context)
                                     (append contexts nil)))))
      (cons (cons 'context context)
            context-settings))))


(provide 'kubernetes-kubectl)

;;; kubernetes-kubectl.el ends here
