;;; kubernetes-contexts.el --- Rendering for Kubernetes contexts  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)

(require 'kubernetes-kubectl)
(require 'kubernetes-modes)
(require 'kubernetes-process)
(require 'kubernetes-props)
(require 'kubernetes-state)
(require 'kubernetes-utils)
(require 'kubernetes-yaml)


;; Component

(defun kubernetes-contexts--render-current-context (context current-namespace)
  (-let* (((&alist 'name name
                   'context (&alist 'cluster cluster-name
                                    'namespace context-namespace))
           context)
          (context-name (propertize name 'face 'kubernetes-context-name))
          (namespace-in-use (or current-namespace context-namespace "default")))
    `((nav-prop :display-config
                (heading (key-value 12 "Context" ,context-name))
                (key-value 12 "Cluster" ,cluster-name))
      (section (namespace nil)
               (nav-prop (:namespace-name ,namespace-in-use)
                         (key-value 12 "Namespace" ,(propertize namespace-in-use 'face 'kubernetes-namespace)))))))

(defun kubernetes-contexts--render-namespace-only (current-namespace)
  (let ((none (propertize "<none>" 'face 'magit-dimmed)))
    `((heading (nav-prop :display-config (key-value 12 "Context" ,none)))
      (section (namespace nil)
               (nav-prop (:namespace-name ,current-namespace)
                         (key-value 12 "Namespace" ,(propertize current-namespace 'face 'kubernetes-namespace)))))))

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


;; Requests and state management

(kubernetes-state-define-refreshers config kubernetes-kubectl-config-view
  "config view -o json")

(defalias 'kubernetes-contexts-refresh-now 'kubernetes-config-refresh-now)
(defalias 'kubernetes-contexts-refresh 'kubernetes-config-refresh-now)
(defalias 'kubernetes-state-contexts 'kubernetes-state-config)
(defalias 'kubernetes-state-update-contexts 'kubernetes-state-update-config)

;; Displaying config.

;;;###autoload
(defun kubernetes-display-config (config)
  "Display information for CONFIG in a new window."
  (interactive (list (kubernetes-kubectl-await-on-async kubernetes-props (kubernetes-state) #'kubernetes-kubectl-config-view)))
  (select-window
   (display-buffer
    (kubernetes-yaml-make-buffer kubernetes-display-config-buffer-name config))))


(provide 'kubernetes-contexts)

;;; kubernetes-contexts.el ends here
