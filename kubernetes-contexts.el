;;; kubernetes-contexts.el --- Rendering for Kubernetes contexts  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 'kubernetes-state)

(defun kubernetes-contexts--render-current-context (context current-namespace)
  (-let* (((&alist 'name name
                   'context (&alist 'cluster cluster-name
                                    'namespace context-namespace))
           context)
          (context-name (propertize name 'face 'kubernetes-context-name))
          (namespace-in-use (or current-namespace context-namespace)))
    `(nav-prop :display-config
               (heading (copy-prop ,name (key-value 12 "Context" ,context-name)))
               (copy-prop ,cluster-name (key-value 12 "Cluster" ,cluster-name))
               (copy-prop ,namespace-in-use
                          (key-value 12 "Namespace" ,namespace-in-use)))))

(defun kubernetes-contexts--render-namespace-only (current-namespace)
  (let ((none (propertize "<none>" 'face 'magit-dimmed)))
    `(nav-prop :display-config
               (heading (key-value 12 "Context" ,none))
               (copy-prop ,current-namespace (key-value 12 "Namespace" ,current-namespace)))))

(defun kubernetes-contexts--render-fetching ()
  (let ((fetching (propertize "Fetching..." 'face 'kubernetes-progress-indicator)))
    `(heading (key-value 12 "Context" ,fetching))))

(defun kubernetes-contexts-render (state)
  (let ((current-namespace (kubernetes-state-current-namespace state))
        (current-context (kubernetes-state-current-context state)))

    `(section (context-container nil)
              (section (context nil)
                       ,(cond
                         (current-context
                          (kubernetes-contexts--render-current-context current-context current-namespace))
                         (current-namespace
                          (kubernetes-contexts--render-namespace-only current-namespace))
                         (t
                          (kubernetes-contexts--render-fetching)))

                       (padding)))))

(provide 'kubernetes-contexts)

;;; kubernetes-contexts.el ends here
