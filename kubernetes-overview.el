;;; kubernetes-overview.el --- Utilities for managing the overview buffer. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'subr-x)

(require 'kubernetes-ast)
(require 'kubernetes-commands)
(require 'kubernetes-configmaps)
(require 'kubernetes-contexts)
(require 'kubernetes-deployments)
(require 'kubernetes-errors)
(require 'kubernetes-jobs)
(require 'kubernetes-loading-container)
(require 'kubernetes-modes)
(require 'kubernetes-namespaces)
(require 'kubernetes-pods)
(require 'kubernetes-pod-line)
(require 'kubernetes-popups)
(require 'kubernetes-secrets)
(require 'kubernetes-services)
(require 'kubernetes-timers)

(autoload 'kubernetes-utils-up-to-existing-dir "kubernetes-utils")


;; Configmaps

(defun kubernetes-overview--referenced-configmaps (state pod)
  (-let* (((&alist 'items configmaps) (kubernetes-state-configmaps state))
          (configmaps (append configmaps nil))
          ((&alist 'spec (&alist 'volumes volumes 'containers containers)) pod)

          (names-in-volumes
           (->> volumes
                (seq-mapcat
                 (lambda (volume)
                   (-when-let ((&alist 'configMap (&alist 'name name)) volume)
                     (list name))))))

          (names-in-env
           (->> containers
                (seq-mapcat (-lambda ((&alist 'env env)) env))
                (seq-mapcat
                 (lambda (env)
                   (-when-let ((&alist 'valueFrom (&alist 'configMapKeyRef (&alist 'name name))) env)
                     (list name))))))

          (references (-uniq (-union names-in-volumes names-in-env))))

    (seq-filter (-lambda ((&alist 'metadata (&alist 'name name)))
                  (member name references))
                configmaps)))

(defun kubernetes-overview--configmaps-for-deployment (state pods)
  (->> pods
       (seq-mapcat (lambda (pod) (kubernetes-overview--referenced-configmaps state pod)))
       -non-nil
       -uniq
       (seq-sort (lambda (s1 s2)
                   (string< (kubernetes-state-resource-name s1)
                            (kubernetes-state-resource-name s2))))))

(kubernetes-ast-define-component aggregated-configmap-line (state configmap)
  (-let* ((pending-deletion (kubernetes-state-configmaps-pending-deletion state))
          (marked-configmaps (kubernetes-state-marked-configmaps state))
          ((&alist 'metadata (&alist 'name name )) configmap)
          (line (cond
                 ((member name pending-deletion)
                  `(propertize (face kubernetes-pending-deletion) ,name))
                 ((member name marked-configmaps)
                  `(mark-for-delete ,name))
                 (t
                  name))))
    `(section (,(intern (kubernetes-state-resource-name configmap)) t)
              (nav-prop (:configmap-name ,name)
                        (copy-prop ,name (line ,line))))))

(kubernetes-ast-define-component aggregated-configmaps (state configmaps)
  `(section (configmaps nil)
            (heading "Configmaps")
            (indent ,(--map `(aggregated-configmap-line ,state ,it) configmaps))
            (padding)))


;; Secrets

(defun kubernetes-overview--referenced-secrets (secrets pod)
  (-let* (((&alist 'spec (&alist 'volumes vols 'containers containers)) pod)
          (combined-env (seq-mapcat (-lambda ((&alist 'env env))
                                      env)
                                    containers))
          (names-in-volumes
           (seq-mapcat
            (lambda (volume)
              (-when-let ((&alist 'secret (&alist 'secretName name)) volume)
                (list name)))
            vols))

          (names-in-env
           (seq-mapcat
            (lambda (env)
              (-when-let ((&alist 'valueFrom (&alist 'secretKeyRef (&alist 'name name))) env)
                (list name)))
            combined-env))

          (references (-union names-in-volumes names-in-env))
          (matches (seq-filter (lambda (secret)
                                 (member (kubernetes-state-resource-name secret) references))
                               secrets)))
    (seq-sort (lambda (s1 s2)
                (string< (kubernetes-state-resource-name s1)
                         (kubernetes-state-resource-name s2)))
              matches)))

(defun kubernetes-overview--secrets-for-deployment (state pods)
  (-let* (((&alist 'items secrets) (kubernetes-state-secrets state))
          (secrets (append secrets nil)))
    (-non-nil (-uniq (seq-mapcat (lambda (pod)
                                   (kubernetes-overview--referenced-secrets secrets pod))
                                 pods)))))

(kubernetes-ast-define-component aggregated-secret-line (state secret)
  (-let* ((pending-deletion (kubernetes-state-secrets-pending-deletion state))
          (marked-secrets (kubernetes-state-marked-secrets state))
          ((&alist 'metadata (&alist 'name name )) secret)
          (line (cond
                 ((member name pending-deletion)
                  `(propertize (face kubernetes-pending-deletion) ,name))
                 ((member name marked-secrets)
                  `(mark-for-delete ,name))
                 (t
                  name))))
    `(section (,(intern (kubernetes-state-resource-name secret)) t)
              (nav-prop (:secret-name ,name)
                        (copy-prop ,name (line ,line))))))

(kubernetes-ast-define-component aggregated-secrets (state secrets)
  `(section (secrets nil)
            (heading "Secrets")
            (indent ,(--map `(aggregated-secret-line ,state ,it) secrets))
            (padding)))


;; Pods

(defun kubernetes-overview--pods-for-deployment (state deployment)
  (-let* (((&alist 'spec (&alist 'selector (&alist 'matchLabels selectors))) deployment)
          ((&alist 'items pods) (kubernetes-state-pods state))
          (pods (append pods nil)))
    (nreverse (seq-reduce
               (lambda (acc pod)
                 (-let [(&alist 'metadata (&alist 'labels labels)) pod]
                   ;; The labels present on the pod must contain all selector labels
                   (if (-all? (lambda (label) (-contains? labels label)) selectors)
                       (cons pod acc)
                     acc)))
               pods
               nil))))

(kubernetes-ast-define-component aggregated-pods (state deployment pods)
  (-let [(&alist 'spec (&alist
                        'replicas replicas
                        'selector (&alist 'matchLabels
                                          (&alist 'name selector-name
                                                  'component component-name)
                                          'matchExpressions match-expressions)))
         deployment]
    `(section (pods nil)
              (heading "Pods")
              (indent
               ,(when selector-name
                  `(section (selector nil)
                            (nav-prop (:selector ,selector-name)
                                      (key-value 12 "Selector" ,(propertize selector-name 'face 'kubernetes-selector)))))
               ,(when component-name
                  `(section (component nil)
                            (nav-prop (:component ,component-name)
                                      (key-value 12 "Component" ,(propertize component-name 'face 'kubernetes-component)))))
               ,(when match-expressions
                  `(section (expressions nil)
                            (heading "Match Expressions")
                            (indent ,(kubernetes-yaml-render match-expressions))))

               (key-value 12 "Replicas" ,(format "%s" (or replicas 1)))

               (loading-container ,(kubernetes-state-pods state)
                                  ,(seq-map (lambda (pod) `(pod-line ,state ,pod)) pods)))

              (padding))))


;; Deployment

(kubernetes-ast-define-component aggregated-deployment-detail (deployment)
  (-let [(&alist 'metadata (&alist 'namespace ns 'creationTimestamp time)
                 'spec (&alist
                        'paused paused
                        'strategy (&alist
                                   'type strategy-type
                                   'rollingUpdate rolling-update)))
         deployment]
    `(,(when paused `(line (propertize (face warning) "Deployment Paused")))
      (section (namespace nil)
               (nav-prop (:namespace-name ,ns)
                         (key-value 12 "Namespace" ,(propertize ns 'face 'kubernetes-namespace))))
      ,(-if-let ((&alist 'maxSurge surge 'maxUnavailable unavailable) rolling-update)
           `(section (strategy t)
                     (heading (key-value 12 "Strategy" ,strategy-type))
                     (indent
                      ((key-value 12 "Max Surge" ,(format "%s" surge))
                       (key-value 12 "Max Unavailable" ,(format "%s" unavailable)))))
         `(key-value 12 "Strategy" ,strategy-type))
      (key-value 12 "Created" ,time))))

(kubernetes-ast-define-component aggregated-deployment (state deployment)
  (let* ((pods (kubernetes-overview--pods-for-deployment state deployment))
         (configmaps (kubernetes-overview--configmaps-for-deployment state pods))
         (secrets (kubernetes-overview--secrets-for-deployment state pods)))
    `(section (,(intern (kubernetes-state-resource-name deployment)) t)
              (heading (deployment-line ,state ,deployment))
              (section (details nil)
                       (indent
                        (aggregated-deployment-detail ,deployment)
                        (padding)
                        (aggregated-pods ,state ,deployment ,pods)
                        ,(when configmaps
                           `(aggregated-configmaps ,state ,configmaps))
                        ,(when secrets
                           `(aggregated-secrets ,state ,secrets)))))))


;; Main Components

(kubernetes-ast-define-component aggregated-view (state &optional hidden)
  (-let [(state-set-p &as &alist 'items deployments) (kubernetes-state-deployments state)]
    `(section (overview-container ,hidden)
              (header-with-count "Deployments" ,deployments)
              (indent
               (columnar-loading-container ,deployments
                                           ,kubernetes-deployments--column-heading
                                           ,@(--map `(aggregated-deployment ,state ,it) deployments)))
              (padding))))

(defun kubernetes-overview-render (state)
  (let ((sections (kubernetes-state-overview-sections state)))
    `(section (root nil)
              ,(kubernetes-errors-render state)
              ,(when (member 'context sections)
                 (kubernetes-contexts-render state))
              ,(when (member 'configmaps sections)
                 `(configmaps-list ,state))
              ,(when (member 'deployments sections)
                 `(deployments-list ,state))
              ,(when (member 'jobs sections)
                 `(jobs-list ,state))
              ,(when (member 'overview sections)
                 `(aggregated-view ,state))
              ,(when (member 'pods sections)
                 `(pods-list ,state))
              ,(when (member 'secrets sections)
                 `(secrets-list ,state))
              ,(when (member 'services sections)
                 `(services-list ,state)))))


;; Overview buffer.

(defun kubernetes-overview--redraw-buffer ()
  "Redraws the main buffer using the current state."
  (when-let (buf (get-buffer kubernetes-overview-buffer-name))
    (with-current-buffer buf
      ;; If a region is active, a redraw would affect the region in
      ;; unpredictable ways.
      (unless (region-active-p)
        ;; Suppress redrawing if the overview is not selected. This prevents
        ;; point from jumping around when a magit popup is open.
        (when (member (selected-window) (get-buffer-window-list buf))
          (kubernetes-utils--save-window-state
           (let ((inhibit-read-only t))
             (erase-buffer)
             (kubernetes-ast-eval (kubernetes-overview-render (kubernetes-state)))))

          ;; Force the section at point to highlight.
          (magit-section-update-highlight))))))

(defun kubernetes-overview--poll (&optional verbose)
  (kubernetes-configmaps-refresh verbose)
  (kubernetes-contexts-refresh verbose)
  (kubernetes-jobs-refresh verbose)
  (kubernetes-deployments-refresh verbose)
  (kubernetes-namespaces-refresh verbose)
  (kubernetes-pods-refresh verbose)
  (kubernetes-secrets-refresh verbose)
  (kubernetes-services-refresh verbose))

(defun kubernetes-overview--initialize-buffer ()
  "Called the first time the overview buffer is opened to set up the buffer."
  (let ((buf (get-buffer-create kubernetes-overview-buffer-name)))
    (with-current-buffer buf
      (kubernetes-overview-mode)
      (add-hook 'kubernetes-redraw-hook #'kubernetes-overview--redraw-buffer)
      (add-hook 'kubernetes-poll-hook #'kubernetes-overview--poll)
      (kubernetes-timers-initialize-timers)
      (kubernetes-overview--redraw-buffer)
      (add-hook 'kill-buffer-hook (kubernetes-utils-make-cleanup-fn buf) nil t))
    buf))

(defun kubernetes-overview-set-sections (sections)
  "Set which sections are displayed in the overview.

SECTIONS is a list of sections to display.  See
`kubernetes-overview-custom-views-alist' and
`kubernetes-overview-views-alist' for possible values."
  (interactive
   (let* ((views (append kubernetes-overview-custom-views-alist kubernetes-overview-views-alist))
          (names (-uniq (--map (symbol-name (car it)) views)))
          (choice (intern (completing-read "Overview view: " names nil t))))
     (list (alist-get choice views))))

  (kubernetes-state-update-overview-sections sections)
  (kubernetes-state-trigger-redraw))

(defvar kubernetes-overview-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "v") #'kubernetes-overview-set-sections)
    keymap)
  "Keymap for `kubernetes-overview-mode'.")

;;;###autoload
(define-derived-mode kubernetes-overview-mode kubernetes-mode "Kubernetes Overview"
  "Mode for working with Kubernetes overview.

\\<kubernetes-overview-mode-map>\
Type \\[kubernetes-overview-set-sections] to choose which resources to display.

Type \\[kubernetes-mark-for-delete] to mark an object for deletion, and \\[kubernetes-execute-marks] to execute.
Type \\[kubernetes-unmark] to unmark the object at point, or \\[kubernetes-unmark-all] to unmark all objects.

Type \\[kubernetes-navigate] to inspect the object on the current line.

Type \\[kubernetes-copy-thing-at-point] to copy the thing at point.

Type \\[kubernetes-refresh] to refresh the buffer.

\\{kubernetes-overview-mode-map}"
  :group 'kubernetes)

;;;###autoload
(defun kubernetes-overview ()
  "Display an overview buffer for Kubernetes."
  (interactive)
  (let ((dir default-directory)
        (buf (kubernetes-overview--initialize-buffer)))
    (when kubernetes-default-overview-namespace
      (kubernetes-set-namespace kubernetes-default-overview-namespace
				(kubernetes-state)))
    (kubernetes-commands-display-buffer buf)
    (with-current-buffer buf
      (cd (kubernetes-utils-up-to-existing-dir dir)))
    (message (substitute-command-keys "\\<kubernetes-overview-mode-map>Type \\[kubernetes-overview-set-sections] to switch between resources, and \\[kubernetes-overview-popup] for usage."))))


(provide 'kubernetes-overview)

;;; kubernetes-overview.el ends here
