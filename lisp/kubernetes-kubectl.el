;;; kubernetes-kubectl.el --- Integration with kubectl.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'kubernetes-custom)

(require 'dash)
(require 'json)
(require 'subr-x)

(defun kubernetes-kubectl--call (&rest args)
  (string-join (apply 'process-lines kubernetes-kubectl-program args) "\n"))

(defun kubernetes-kubectl-current-namespace ()
  "Return the current namespace for the active Kubernetes context."
  (alist-get 'namespace (kubernetes-kubectl-kubeconfig-settings)))

(defun kubernetes-kubectl-known-namespaces ()
  (let ((config (json-read-from-string (kubernetes-kubectl--call "config" "view" "-o" "json"))))
    (-when-let ((&alist 'contexts contexts) config)
      (->> (append contexts nil)
           (-keep (-lambda ((&alist 'context (&alist 'namespace ns))) ns))
           (-uniq)
           (-sort #'string<)))))

(defun kubernetes-kubectl-kubeconfig-settings ()
  "Return an alist of settings for the active Kubernetes context."
  (let ((config (json-read-from-string (kubernetes-kubectl--call "config" "view" "-o" "json"))))

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
