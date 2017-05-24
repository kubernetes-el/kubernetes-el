;;; kubernetes-pods-list.el --- Displays pods.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)

(require 'kubernetes-ast)

(kubernetes-ast-define-component pod-list-entry (pod)
  (-let [(&alist 'metadata (&alist 'name name)) pod]
    `(section (pod-entry t)
              (heading ,name))))

(kubernetes-ast-define-component pods-list (state)
  (-let [(&hash 'pods pods) state]
    `(section (pods-list nil)
              (heading "Pods")
              (indent
               ,(--map `(pod-list-entry ,(gethash it pods))
                       (kubernetes-pods-list--sorted-keys pods)))
              (padding))))

(defun kubernetes-pods-list--sorted-keys (ht)
  (-sort (lambda (l r) (string< (symbol-name l) (symbol-name r)))
         (hash-table-keys ht)))

(provide 'kubernetes-pods-list)

;;; kubernetes-pods-list.el ends here
