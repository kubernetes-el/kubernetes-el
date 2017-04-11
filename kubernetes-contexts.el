;;; kubernetes-contexts.el --- Rendering for Kubernetes contexts  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 'kubernetes-state)

(defun kubernetes-contexts-render (state)
  `(section (context-container nil)
            (section (context nil)
                     ,(-let* ((current-namespace (kubernetes-state-current-namespace state))
                              (config (kubernetes-state-config))
                              (current-context (kubernetes-state-current-context)))
                        (cond

                         ;; If a context is selected, draw that.
                         ((and config current-context)
                          (-let* (((&alist 'name name 'context (&alist 'cluster cluster-name 'namespace ns)) current-context)
                                  (context-name (propertize name 'face 'kubernetes-context-name)))
                            `(nav-prop :display-config
                                       ((heading (copy-prop ,name (key-value 12 "Context" ,context-name)))
                                        (copy-prop ,cluster-name (key-value 12 "Cluster" ,cluster-name))
                                        (copy-prop ,(or current-namespace ns)
                                                   (key-value 12 "Namespace" ,(or current-namespace ns)))))))

                         ;; If there is no context, draw the namespace.
                         (current-namespace
                          (let ((none (propertize "<none>" 'face 'magit-dimmed)))
                            `(nav-prop :display-config
                                       ((heading (key-value 12 "Context" ,none))
                                        (copy-prop ,current-namespace (key-value 12 "Namespace" ,current-namespace))))))

                         ;; If state is empty, assume requests are in progress.
                         (t
                          (let ((fetching (propertize "Fetching..." 'face 'kubernetes-progress-indicator)))
                            `(heading (key-value 12 "Context" ,fetching))))))

                     (padding))))


(provide 'kubernetes-contexts)

;;; kubernetes-contexts.el ends here
