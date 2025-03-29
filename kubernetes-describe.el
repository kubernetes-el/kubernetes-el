;;; kubernetes-describe.el --- Support for describing all Kubernetes resources  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'kubernetes-commands)
(require 'kubernetes-kubectl)
(require 'kubernetes-state)
(require 'kubernetes-utils)
(require 'kubernetes-vars)

;; Constants

(defconst kubernetes-describable-resources
  '("configmap" "deployment" "ingress" "job" "namespace" "node"
    "persistentvolumeclaim" "pod" "secret" "service" "statefulset"
    "networkpolicy" "cronjob")
  "List of Kubernetes resource types that support `kubectl describe`.")

(defvar kubernetes-describe-buffer-name-format "*kubernetes describe %s%s/%s*"
  "Format for Kubernetes describe buffer names.
First %s is replaced with the namespace prefix if specified (including trailing slash),
second %s is the resource type,
third %s is the resource name.")

(defun kubernetes-describe--generate-buffer-name (resource-type resource-name state)
  "Generate a buffer name for describing RESOURCE-TYPE named RESOURCE-NAME in STATE.
Includes namespace information from STATE when available."
  (let* ((namespace (kubernetes-state--get state 'current-namespace))
         (namespace-prefix (if namespace (format "%s/" namespace) "")))
    (format kubernetes-describe-buffer-name-format namespace-prefix resource-type resource-name)))

;; Backend functions

(defun kubernetes-kubectl-describe-resource (state resource-type resource-name cb)
  "Run kubectl describe for a resource of RESOURCE-TYPE with RESOURCE-NAME.

STATE is the current application state.
CB is a function taking the output."
  (kubernetes-kubectl state (list "describe" resource-type resource-name)
                     (lambda (buf)
                       (let ((s (with-current-buffer buf (buffer-string))))
                         (funcall cb s)))))

;; Helper functions

(defun kubernetes-describe--validate-resource-type (resource-type)
  "Validate that RESOURCE-TYPE is describable.
Signal an error if the resource type is not supported."
  (unless (member resource-type kubernetes-describable-resources)
    (let ((singular-type (if (string-suffix-p "s" resource-type)
                             (substring resource-type 0 -1)
                           resource-type)))
      ;; Check both plural and singular forms
      (unless (member singular-type kubernetes-describable-resources)
        (user-error "Resource type %s is not describable" resource-type)))))

(defun kubernetes-describe--get-resource-name (state resource-type)
  "Get a resource name of RESOURCE-TYPE using STATE.
Attempts to use specific read functions when available,
otherwise fetches and displays a list of available resources.
RESOURCE-TYPE must be one of the types in `kubernetes-describable-resources'."
  ;; Validate resource type
  (kubernetes-describe--validate-resource-type resource-type)

  ;; Normalize resource-type to singular form for function lookup
  (let* ((normalized-type (if (string-suffix-p "s" resource-type)
                             (substring resource-type 0 -1)
                           resource-type))
         ;; Try to determine the read function name
         (read-fn-name (format "kubernetes-%ss--read-name" normalized-type))
         (read-fn (condition-case nil
                     (intern-soft read-fn-name)
                   (error nil))))

    ;; If we have a specific read function, use it
    (if (and read-fn (fboundp read-fn))
        (funcall read-fn state)
      ;; Otherwise, use the common resource selection mechanism
      (kubernetes-utils-get-resource-name state resource-type))))

;;;###autoload
(defun kubernetes-describe-generic-resource (resource-type)
  "Display a buffer for describing a Kubernetes RESOURCE-TYPE.
Prompts for the resource name, with a default from point if available."
  (interactive
   (list (completing-read "Resource type: " kubernetes-describable-resources nil t)))
  (let* ((get-name-at-point-fn
          (intern-soft (format "kubernetes-utils-maybe-%s-name-at-point"
                              (if (string= resource-type "persistentvolumeclaim")
                                  "pvc"
                                resource-type))))
         (read-name-fn
          (intern-soft (format "kubernetes-%ss--read-name"
                              (if (string= resource-type "persistentvolumeclaim")
                                  "persistentvolumeclaim"
                                resource-type))))
         (name-at-point (when (fboundp get-name-at-point-fn)
                          (funcall get-name-at-point-fn)))
         (resource-name (or name-at-point
                           (if (and read-name-fn (fboundp read-name-fn))
                               (funcall read-name-fn (kubernetes-state))
                             (kubernetes-describe--get-resource-name (kubernetes-state) resource-type)))))
    (kubernetes-describe-resource resource-type resource-name)))

;; Main functions

;;;###autoload
(defun kubernetes-describe-dwim ()
  "Describe the resource at point.

The resource at point must be a valid target for `kubectl describe'."
  (interactive)
  (if-let ((resource-info (kubernetes-utils-get-resource-info-at-point)))
      (let ((resource-type (car resource-info))
            (resource-name (cdr resource-info)))
        (if (member resource-type kubernetes-describable-resources)
            (kubernetes-describe-resource resource-type resource-name)
          (user-error "Resource type %s is not describable" resource-type)))
    (user-error "No describable resource at point")))

(defun kubernetes-describe-resource (resource-type resource-name)
  "Display a buffer for describing a Kubernetes resource.

RESOURCE-TYPE is the type of resource (e.g., 'deployment', 'service').
RESOURCE-NAME is the name of the resource to describe."
  (interactive
   (let* ((state (kubernetes-state))
          (resource-info (kubernetes-utils-get-resource-info-at-point)))
     (if resource-info
         (list (car resource-info) (cdr resource-info))
       (let* ((type (completing-read "Resource type: " kubernetes-describable-resources nil t))
              (name (kubernetes-describe--get-resource-name state type)))
         (list type name)))))

  (let* ((state (kubernetes-state))
         (buffer-name (kubernetes-describe--generate-buffer-name resource-type resource-name state))
         (buf (get-buffer-create buffer-name))
         (marker (make-marker)))
    (with-current-buffer buf
      (kubernetes-display-thing-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (set-marker marker (point))
        (insert (propertize (format "Loading %s %s..." resource-type resource-name) 'face 'kubernetes-dimmed))))
    (let* ((populate-buffer (lambda (s)
                              (with-current-buffer (marker-buffer marker)
                                (setq-local tab-width 8)
                                (let ((inhibit-read-only t)
                                      (inhibit-redisplay t))
                                  (erase-buffer)
                                  (insert "---\n")
                                  (insert s)
                                  (untabify (point-min) (point-max))
                                  (goto-char (point-min))))))
           (proc (kubernetes-kubectl-describe-resource state
                                                      resource-type
                                                      resource-name
                                                      populate-buffer)))
      (with-current-buffer buf
        ;; Store resource information in buffer-local variables for potential reuse
        (setq-local kubernetes-describe-resource-type resource-type)
        (setq-local kubernetes-describe-resource-name resource-name)
        (setq-local kubernetes-describe-namespace (kubernetes-state--get state 'current-namespace))

        (add-hook 'kill-buffer-hook (lambda () (kubernetes-process-kill-quietly proc)) nil t)))

    (select-window (display-buffer buf))
    buf))

;; For backward compatibility, define aliases for commonly used resource functions
;;;###autoload
(defalias 'kubernetes-describe-pod
  (lambda (pod-name)
    (interactive (list (or (kubernetes-utils-maybe-pod-name-at-point)
                          (kubernetes-pods--read-name (kubernetes-state)))))
    (kubernetes-describe-resource "pod" pod-name))
  "Display a buffer for describing a pod.
POD-NAME is the name of the pod to describe.")

;;;###autoload
(defalias 'kubernetes-describe-deployment
  (lambda (deployment-name)
    (interactive (list (or (kubernetes-utils-maybe-deployment-name-at-point)
                          (kubernetes-deployments--read-name (kubernetes-state)))))
    (kubernetes-describe-resource "deployment" deployment-name))
  "Display a buffer for describing a deployment.
DEPLOYMENT-NAME is the name of the deployment to describe.")

;;;###autoload
(defalias 'kubernetes-describe-service
  (lambda (service-name)
    (interactive (list (or (kubernetes-utils-get-resource-name-at-point "service")
                          (kubernetes-services--read-name (kubernetes-state)))))
    (kubernetes-describe-resource "service" service-name))
  "Display a buffer for describing a service.
SERVICE-NAME is the name of the service to describe.")

;;;###autoload
(defalias 'kubernetes-describe-node
  (lambda (node-name)
    (interactive (list (or (kubernetes-utils-get-resource-name-at-point "node")
                          (kubernetes-nodes--read-name (kubernetes-state)))))
    (kubernetes-describe-resource "node" node-name))
  "Display a buffer for describing a node.
NODE-NAME is the name of the node to describe.")

;;;###autoload
(defalias 'kubernetes-describe-statefulset
  (lambda (statefulset-name)
    (interactive (list (or (kubernetes-utils-maybe-statefulset-name-at-point)
                          (kubernetes-statefulsets--read-name (kubernetes-state)))))
    (kubernetes-describe-resource "statefulset" statefulset-name))
  "Display a buffer for describing a statefulset.
STATEFULSET-NAME is the name of the statefulset to describe.")

;;;###autoload
(defalias 'kubernetes-describe-job
  (lambda (job-name)
    (interactive (list (or (kubernetes-utils-maybe-job-name-at-point)
                          (kubernetes-jobs--read-name (kubernetes-state)))))
    (kubernetes-describe-resource "job" job-name))
  "Display a buffer for describing a job.
JOB-NAME is the name of the job to describe.")

(provide 'kubernetes-describe)

;;; kubernetes-describe.el ends here
