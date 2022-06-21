;;; kubernetes-contexts.el --- Rendering for Kubernetes contexts  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)

(require 'kubernetes-kubectl)
(require 'kubernetes-modes)
(require 'kubernetes-process)
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
                (key-value 12 "Cluster" ,cluster-name)
                (proxy-status))
      (section (namespace nil)
               (nav-prop (:namespace-name ,namespace-in-use)
                         (key-value 12 "Namespace" ,(propertize namespace-in-use 'face 'kubernetes-namespace)))))))

(defun kubernetes-contexts--render-namespace-only (current-namespace)
  (let ((none (propertize "<none>" 'face 'kubernetes-dimmed)))
    `((heading (nav-prop :display-config (key-value 12 "Context" ,none)))
      (section (namespace nil)
               (nav-prop (:namespace-name ,current-namespace)
                         (key-value 12 "Namespace" ,(propertize current-namespace 'face 'kubernetes-namespace)))))))

(defun kubernetes-contexts--render-fetching ()
  (let ((fetching (propertize "Fetching..." 'face 'kubernetes-progress-indicator)))
    `(heading (key-value 12 "Context" ,fetching))))

(defun kubernetes-contexts-render (state)
  (let ((current-namespace (kubernetes-state--get state 'current-namespace))
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
(defun kubernetes-state-contexts (state)
  (kubernetes-state--get state 'config))
(defalias 'kubernetes-state-update-contexts 'kubernetes-state-update-config)

;; Displaying config.

;;;###autoload
(defun kubernetes-display-config (config)
  "Display information for CONFIG in a new window."
  (interactive (list (kubernetes-kubectl-await-on-async (kubernetes-state) #'kubernetes-kubectl-config-view)))
  (select-window
   (display-buffer
    (kubernetes-yaml-make-buffer kubernetes-display-config-buffer-name config))))

(defun kubernetes-contexts--context-names (state)
  "Get a list of all available kubectl contexts from STATE."
  (-let* ((config (or (kubernetes-state--get state 'config) (kubernetes-kubectl-await-on-async state #'kubernetes-kubectl-config-view)))
          ((&alist 'contexts contexts) config))
    (--map (alist-get 'name it) contexts)))

(defun kubernetes-contexts-use-context (context)
  "Switch Kubernetes context refresh the pods buffer.

CONTEXT is the name of a context as a string."
  (interactive (list (completing-read "Context: " (kubernetes-contexts--context-names (kubernetes-state)) nil t)))
  (kubernetes-process-kill-polling-processes)

  (let ((state (kubernetes-state)))
    (kubernetes-state-clear)
    (kubernetes-state-update-overview-sections (kubernetes-state-overview-sections state)))

  (kubernetes-state-trigger-redraw)

  (when-let (buf (get-buffer kubernetes-overview-buffer-name))
    (with-current-buffer buf
      (goto-char (point-min))))

  (let ((state (kubernetes-state)))
    (kubernetes-kubectl-config-use-context
     state
     context
     (lambda (_)
       (when kubernetes-default-overview-namespace
         (kubernetes-set-namespace kubernetes-default-overview-namespace
                                   state))
       (kubernetes-state-trigger-redraw)))))

(defun kubernetes-contexts-rename (context new-name)
  "Renames CONTEXT to NEW-NAME.

If CONTEXT is the current context, reloads."
  (interactive
   (let* ((contexts (kubernetes-contexts--context-names (kubernetes-state)))
          (context-to-rename (completing-read "Rename context: " contexts)))
     (list context-to-rename
           (read-string (format "Rename `%s' to: " context-to-rename)))))

  (let ((contexts (kubernetes-contexts--context-names (kubernetes-state))))
    (when (not (-contains-p contexts context))
      (error "Context `%s' does not exist" context-to-rename))
    (when (-contains-p contexts new-name)
      (error "Already exists a context named `%s'" new-name)))

  (-let* (((&alist 'name current-context)
           (kubernetes-state-current-context (kubernetes-state))))
    (kubernetes-kubectl
     (kubernetes-state)
     `("config" "rename-context" ,context ,new-name)
     (lambda (_)
       (message "Renamed context `%s' to `%s'." context new-name)
       (kubernetes-contexts-refresh-now)
       (when (string-equal context current-context)
         (kubernetes-contexts-use-context new-name))))))

(provide 'kubernetes-contexts)

;;; kubernetes-contexts.el ends here
