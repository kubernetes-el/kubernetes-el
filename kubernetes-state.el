;;; kubernetes-state.el --- Main state for Kubernetes  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(require 'kubernetes-vars)


;;; Main state

(defvar kubernetes-state--current-state nil)

(defun kubernetes-state ()
  kubernetes-state--current-state)

(defun kubernetes-state-update (action &optional args)
  (let ((updated (kubernetes-state-next kubernetes-state--current-state action args)))
    (setq kubernetes-state--current-state updated)))

(defun kubernetes-state-next (state action &optional args)
  (let ((next (copy-alist state)))
    (pcase action

      (:update-current-time
       (setf (alist-get 'current-time next) args))
      (:update-last-error
       (setf (alist-get 'last-error next) args))
      (:update-label-query
       (setf (alist-get 'label-query next) args))
      (:update-namespaces
       (setf (alist-get 'namespaces next) args))
      (:update-current-namespace
       (setf (alist-get 'current-namespace next) args))
      (:update-kubectl-flags
       (setf (alist-get 'kubectl-flags next) args))

      (:update-config
       (setf (alist-get 'config next) args)
       (unless (alist-get 'current-namespace next)
         (-when-let ((&alist 'context (&alist 'namespace ns))
                     (kubernetes-state--lookup-current-context args))
           (setf (alist-get 'current-namespace next) ns))))

      (:unmark-all
       (setf (alist-get 'marked-configmaps next nil t) nil)
       (setf (alist-get 'marked-deployments next nil t) nil)
       (setf (alist-get 'marked-pods next nil t) nil)
       (setf (alist-get 'marked-secrets next nil t) nil)
       (setf (alist-get 'marked-services next nil t) nil))

      ;; Pods

      (:mark-pod
       (let ((cur (alist-get 'marked-pods state)))
         (setf (alist-get 'marked-pods next)
               (delete-dups (cons args cur)))))
      (:unmark-pod
       (setf (alist-get 'marked-pods next)
             (remove args (alist-get 'marked-pods next))))
      (:delete-pod
       (let ((updated (cons args (alist-get 'pods-pending-deletion state))))
         (setf (alist-get 'pods-pending-deletion next)
               (delete-dups updated))))
      (:update-pods
       (setf (alist-get 'pods next) args)
       ;; Prune deleted pods from state.
       (-let* (((&alist 'items pods) args)
               (pod-names (seq-map #'kubernetes-state-resource-name pods)))
         (setf (alist-get 'marked-pods next)
               (seq-intersection (alist-get 'marked-pods next)
                                 pod-names))
         (setf (alist-get 'pods-pending-deletion next)
               (seq-intersection (alist-get 'pods-pending-deletion next)
                                 pod-names))))

      ;; Configmaps

      (:mark-configmap
       (let ((cur (alist-get 'marked-configmaps state)))
         (setf (alist-get 'marked-configmaps next)
               (delete-dups (cons args cur)))))
      (:unmark-configmap
       (setf (alist-get 'marked-configmaps next)
             (remove args (alist-get 'marked-configmaps next))))
      (:delete-configmap
       (let ((updated (cons args (alist-get 'configmaps-pending-deletion state))))
         (setf (alist-get 'configmaps-pending-deletion next)
               (delete-dups updated))))
      (:update-configmaps
       (setf (alist-get 'configmaps next) args)

       ;; Prune deleted configmaps from state.
       (-let* (((&alist 'items configmaps) args)
               (configmap-names (seq-map #'kubernetes-state-resource-name configmaps)))
         (setf (alist-get 'marked-configmaps next)
               (seq-intersection (alist-get 'marked-configmaps next)
                                 configmap-names))
         (setf (alist-get 'configmaps-pending-deletion next)
               (seq-intersection (alist-get 'configmaps-pending-deletion next)
                                 configmap-names))))

      ;; Secrets

      (:mark-secret
       (let ((cur (alist-get 'marked-secrets state)))
         (setf (alist-get 'marked-secrets next)
               (delete-dups (cons args cur)))))
      (:unmark-secret
       (setf (alist-get 'marked-secrets next)
             (remove args (alist-get 'marked-secrets next))))
      (:delete-secret
       (let ((updated (cons args (alist-get 'secrets-pending-deletion state))))
         (setf (alist-get 'secrets-pending-deletion next)
               (delete-dups updated))))
      (:update-secrets
       (setf (alist-get 'secrets next) args)

       ;; Prune deleted secrets from state.
       (-let* (((&alist 'items secrets) args)
               (secret-names (seq-map #'kubernetes-state-resource-name secrets)))
         (setf (alist-get 'marked-secrets next)
               (seq-intersection (alist-get 'marked-secrets next)
                                 secret-names))
         (setf (alist-get 'secrets-pending-deletion next)
               (seq-intersection (alist-get 'secrets-pending-deletion next)
                                 secret-names))))


      ;; Services

      (:mark-service
       (let ((cur (alist-get 'marked-services state)))
         (setf (alist-get 'marked-services next)
               (delete-dups (cons args cur)))))
      (:unmark-service
       (setf (alist-get 'marked-services next)
             (remove args (alist-get 'marked-services next))))
      (:delete-service
       (let ((updated (cons args (alist-get 'services-pending-deletion state))))
         (setf (alist-get 'services-pending-deletion next)
               (delete-dups updated))))
      (:update-services
       (setf (alist-get 'services next) args)

       ;; Prune deleted services from state.
       (-let* (((&alist 'items services) args)
               (service-names (seq-map #'kubernetes-state-resource-name services)))
         (setf (alist-get 'marked-services next)
               (seq-intersection (alist-get 'marked-services next)
                                 service-names))
         (setf (alist-get 'services-pending-deletion next)
               (seq-intersection (alist-get 'services-pending-deletion next)
                                 service-names))))

      ;; Deployments

      (:mark-deployment
       (let ((cur (alist-get 'marked-deployments state)))
         (setf (alist-get 'marked-deployments next)
               (delete-dups (cons args cur)))))
      (:unmark-deployment
       (setf (alist-get 'marked-deployments next)
             (remove args (alist-get 'marked-deployments next))))
      (:delete-deployment
       (let ((updated (cons args (alist-get 'deployments-pending-deletion state))))
         (setf (alist-get 'deployments-pending-deletion next)
               (delete-dups updated))))
      (:update-deployments
       (setf (alist-get 'deployments next) args)

       ;; Prune deleted deployments from state.
       (-let* (((&alist 'items deployments) args)
               (deployment-names (seq-map #'kubernetes-state-resource-name deployments)))
         (setf (alist-get 'marked-deployments next)
               (seq-intersection (alist-get 'marked-deployments next)
                                 deployment-names))
         (setf (alist-get 'deployments-pending-deletion next)
               (seq-intersection (alist-get 'deployments-pending-deletion next)
                                 deployment-names))))

      (_
       (error "Unknown action: %s" action)))

    next))

(defun kubernetes-state--lookup-current-context (config)
  (-let [(&alist 'contexts contexts 'current-context current) config]
    (--find (equal current (alist-get 'name it)) (append contexts nil))))

(defun kubernetes-state-clear ()
  (setq kubernetes-state--current-state nil))


;; Actions

(defun kubernetes-state-mark-pod (pod-name)
  (cl-assert (stringp pod-name))
  (kubernetes-state-update :mark-pod pod-name))

(defun kubernetes-state-unmark-pod (pod-name)
  (cl-assert (stringp pod-name))
  (kubernetes-state-update :unmark-pod pod-name))

(defun kubernetes-state-delete-pod (pod-name)
  (cl-assert (stringp pod-name))
  (kubernetes-state-update :delete-pod pod-name)
  (kubernetes-state-update :unmark-pod pod-name))

(defun kubernetes-state-mark-configmap (configmap-name)
  (cl-assert (stringp configmap-name))
  (kubernetes-state-update :mark-configmap configmap-name))

(defun kubernetes-state-unmark-configmap (configmap-name)
  (cl-assert (stringp configmap-name))
  (kubernetes-state-update :unmark-configmap configmap-name))

(defun kubernetes-state-delete-configmap (configmap-name)
  (cl-assert (stringp configmap-name))
  (kubernetes-state-update :delete-configmap configmap-name)
  (kubernetes-state-update :unmark-configmap configmap-name))

(defun kubernetes-state-mark-secret (secret-name)
  (cl-assert (stringp secret-name))
  (kubernetes-state-update :mark-secret secret-name))

(defun kubernetes-state-unmark-secret (secret-name)
  (cl-assert (stringp secret-name))
  (kubernetes-state-update :unmark-secret secret-name))

(defun kubernetes-state-delete-secret (secret-name)
  (cl-assert (stringp secret-name))
  (kubernetes-state-update :delete-secret secret-name)
  (kubernetes-state-update :unmark-secret secret-name))

(defun kubernetes-state-mark-service (service-name)
  (cl-assert (stringp service-name))
  (kubernetes-state-update :mark-service service-name))

(defun kubernetes-state-unmark-service (service-name)
  (cl-assert (stringp service-name))
  (kubernetes-state-update :unmark-service service-name))

(defun kubernetes-state-delete-service (service-name)
  (cl-assert (stringp service-name))
  (kubernetes-state-update :delete-service service-name)
  (kubernetes-state-update :unmark-service service-name))

(defun kubernetes-state-mark-deployment (deployment-name)
  (cl-assert (stringp deployment-name))
  (kubernetes-state-update :mark-deployment deployment-name))

(defun kubernetes-state-unmark-deployment (deployment-name)
  (cl-assert (stringp deployment-name))
  (kubernetes-state-update :unmark-deployment deployment-name))

(defun kubernetes-state-delete-deployment (deployment-name)
  (cl-assert (stringp deployment-name))
  (kubernetes-state-update :delete-deployment deployment-name)
  (kubernetes-state-update :unmark-deployment deployment-name))

(defun kubernetes-state-unmark-all ()
  (kubernetes-state-update :unmark-all))


;; State accessors

(defmacro kubernetes-state--define-getter (attr)
  `(defun ,(intern (format "kubernetes-state-%s" attr)) (state)
     (alist-get (quote ,attr) state)))

(defmacro kubernetes-state--define-setter (attr arglist &rest forms-before-update)
  (declare (indent 2))
  (let ((getter (intern (format "kubernetes-state-%s" attr)))
        (arg
         (pcase arglist
           (`(,x) x)
           (xs `(list ,@xs)))))
    `(defun ,(intern (format "kubernetes-state-update-%s" attr)) ,arglist
       ,@forms-before-update
       (let ((prev (,getter (kubernetes-state)))
             (arg ,arg))
         (kubernetes-state-update ,(intern (format ":update-%s" attr)) ,arg)

         ;; Redraw immediately if this value was previously unset.
         (unless prev
           (kubernetes-state-trigger-redraw))

         arg))))

(defmacro kubernetes-state--define-accessors (attr arglist &rest forms-before-update)
  (declare (indent 2))
  `(progn
     (kubernetes-state--define-getter ,attr)
     (kubernetes-state--define-setter ,attr ,arglist ,@forms-before-update)))

(kubernetes-state--define-accessors current-namespace (namespace)
  (cl-assert (stringp namespace)))

(kubernetes-state--define-accessors pods (pods)
  (cl-assert (listp pods)))

(kubernetes-state--define-accessors configmaps (configmaps)
  (cl-assert (listp configmaps)))

(kubernetes-state--define-accessors secrets (secrets)
  (cl-assert (listp secrets)))

(kubernetes-state--define-accessors services (services)
  (cl-assert (listp services)))

(kubernetes-state--define-accessors deployments (deployments)
  (cl-assert (listp deployments)))

(kubernetes-state--define-accessors namespaces (namespaces)
  (cl-assert (listp namespaces)))

(kubernetes-state--define-accessors config (config)
  (cl-assert (listp config)))

(kubernetes-state--define-accessors label-query (label-name)
  (cl-assert (stringp label-name)))

(defun kubernetes-state-kubectl-flags (state)
  (if-let (flags (alist-get 'kubectl-flags state))
      flags
    (alist-get 'kubectl-flags (kubernetes-state-update :update-kubectl-flags kubernetes-kubectl-flags))))

(kubernetes-state--define-setter kubectl-flags (flags)
  (cl-assert (listp flags))
  (cl-assert (-all? #'stringp flags))
  (setq kubernetes-kubectl-flags flags))

(kubernetes-state--define-getter marked-configmaps)
(kubernetes-state--define-getter configmaps-pending-deletion)

(kubernetes-state--define-getter marked-pods)
(kubernetes-state--define-getter pods-pending-deletion)

(kubernetes-state--define-getter marked-secrets)
(kubernetes-state--define-getter secrets-pending-deletion)

(kubernetes-state--define-getter marked-services)
(kubernetes-state--define-getter services-pending-deletion)

(kubernetes-state--define-getter marked-deployments)
(kubernetes-state--define-getter deployments-pending-deletion)

(kubernetes-state--define-getter last-error)

(defun kubernetes-state-update-last-error (message command time)
  (cl-assert (stringp message))
  (cl-assert (stringp command))
  (cl-assert time)
  (cl-assert (listp time))
  (cl-assert (-all? #'integerp time))
  (let ((arg `((message . ,message)
               (command . ,command)
               (time ., time))))
    (kubernetes-state-update :update-last-error arg)
    arg))

;; No update function is provided. The time is updated internally before the
;; redrawing hook is run.
(kubernetes-state--define-getter current-time)


;; Convenience functions.

(defmacro kubernetes-state-define-named-lookup (resource state-key)
  "Define `kubernetes-state-lookup-RESOURCE' for looking up an item by name.

RESOURCE is the name of the resource.

STATE-KEY is the key to look up an item in the state."
  (let* ((ident (symbol-name resource))
         (docstring
          (format "Look up a %s by name in the current state.

%s-NAME is the name of the %s to search for.

STATE is the current application state.

If lookup succeeds, return the alist representation of the resource.
If lookup fails, return nil."
                  ident (upcase ident) ident)))

    `(defun ,(intern (format "kubernetes-state-lookup-%s" resource)) (name state)
       ,docstring
       (-let [(&alist ',state-key (&alist 'items items)) state]
         (seq-find (lambda (it) (equal (kubernetes-state-resource-name it) name))
                   items)))))

(kubernetes-state-define-named-lookup pod pods)
(kubernetes-state-define-named-lookup configmap configmaps)
(kubernetes-state-define-named-lookup secret secrets)
(kubernetes-state-define-named-lookup service services)
(kubernetes-state-define-named-lookup deployment deployments)
(kubernetes-state-define-named-lookup namespace namespaces)

(defun kubernetes-state-resource-name (resource)
  "Get the name of RESOURCE from its metadata.

RESOURCE is the parsed representation an API resource, such a
pod, secret, configmap, etc."
  (-let [(&alist 'metadata (&alist 'name name)) resource]
    name))

(defun kubernetes-state-current-context (state)
  (when-let (config (kubernetes-state-config state))
    (kubernetes-state--lookup-current-context config)))

(defun kubernetes-state-clear-error-if-stale (error-display-time)
  (-when-let ((&alist 'time err-time) (kubernetes-state-last-error (kubernetes-state)))
    (when (< error-display-time
             (- (time-to-seconds) (time-to-seconds err-time)))
      (kubernetes-state-update :update-last-error nil))))

(defun kubernetes-state-trigger-redraw ()
  (kubernetes-state-update :update-current-time (current-time))
  (kubernetes-state-clear-error-if-stale kubernetes-minimum-error-display-time)
  (run-hooks 'kubernetes-redraw-hook))


(provide 'kubernetes-state)

;;; kubernetes-state.el ends here
