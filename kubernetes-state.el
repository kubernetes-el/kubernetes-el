;;; kubernetes-state.el --- Main state for Kubernetes  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(require 'kubernetes-core)
(require 'kubernetes-process)


;;; Main state

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
      (:update-overview-sections
       (setf (alist-get 'overview-sections next) args))

      (:update-config
       (setf (alist-get 'config next) args)
       (unless (alist-get 'current-namespace next)
         (-when-let ((&alist 'context (&alist 'namespace ns))
                     (kubernetes-state--lookup-current-context args))
           (setf (alist-get 'current-namespace next) ns))))

      (:unmark-all
       (setf (alist-get 'marked-configmaps next nil t) nil)
       (setf (alist-get 'marked-deployments next nil t) nil)
       (setf (alist-get 'marked-statefulsets next nil t) nil)
       (setf (alist-get 'marked-jobs next nil t) nil)
       (setf (alist-get 'marked-persistentvolumeclaims next nil t) nil)
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
               (pod-names (seq-map #'kubernetes-state-resource-name (append pods nil))))
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
               (configmap-names (seq-map #'kubernetes-state-resource-name (append configmaps nil))))
         (setf (alist-get 'marked-configmaps next)
               (seq-intersection (alist-get 'marked-configmaps next)
                                 configmap-names))
         (setf (alist-get 'configmaps-pending-deletion next)
               (seq-intersection (alist-get 'configmaps-pending-deletion next)
                                 configmap-names))))

      ;; Persistent Volume Claims.

      (:mark-persistentvolumeclaim
       (let ((cur (alist-get 'marked-persistentvolumeclaims state)))
         (setf (alist-get 'marked-persistentvolumeclaims next)
               (delete-dups (cons args cur)))))
      (:unmark-persistentvolumeclaim
       (setf (alist-get 'marked-persistentvolumeclaims next)
             (remove args (alist-get 'marked-persistentvolumeclaims next))))
      (:delete-persistentvolumeclaim
       (let ((updated (cons args (alist-get 'persistentvolumeclaims-pending-deletion state))))
         (setf (alist-get 'persistentvolumeclaims-pending-deletion next)
               (delete-dups updated))))
      (:update-persistentvolumeclaims
       (setf (alist-get 'persistentvolumeclaims next) args)

       ;; Prune deleted persistentvolumeclaims from state.
       (-let* (((&alist 'items persistentvolumeclaims) args)
               (persistentvolumeclaim-names (seq-map #'kubernetes-state-resource-name (append persistentvolumeclaims nil))))
         (setf (alist-get 'marked-persistentvolumeclaims next)
               (seq-intersection (alist-get 'marked-persistentvolumeclaims next)
                                 persistentvolumeclaim-names))
         (setf (alist-get 'persistentvolumeclaims-pending-deletion next)
               (seq-intersection (alist-get 'persistentvolumeclaims-pending-deletion next)
                                 persistentvolumeclaim-names))))

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
               (secret-names (seq-map #'kubernetes-state-resource-name (append secrets nil))))
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
               (service-names (seq-map #'kubernetes-state-resource-name (append services nil))))
         (setf (alist-get 'marked-services next)
               (seq-intersection (alist-get 'marked-services next)
                                 service-names))
         (setf (alist-get 'services-pending-deletion next)
               (seq-intersection (alist-get 'services-pending-deletion next)
                                 service-names))))

      ;; Ingress
      (:mark-ingress
       (let ((cur (alist-get 'marked-ingress state)))
         (setf (alist-get 'marked-ingress next)
               (delete-dups (cons args cur)))))
      (:unmark-ingress
       (setf (alist-get 'marked-ingress next)
             (remove args (alist-get 'marked-ingress next))))
      (:delete-ingress
       (let ((updated (cons args (alist-get 'ingress-pending-deletion state))))
         (setf (alist-get 'ingress-pending-deletion next)
               (delete-dups updated))))
      (:update-ingress
       (setf (alist-get 'ingress next) args)

       ;; Prune deleted ingress from state.
       (-let* (((&alist 'items ingress) args)
               (ingress-names (seq-map #'kubernetes-state-resource-name (append ingress nil))))
         (setf (alist-get 'marked-ingress next)
               (seq-intersection (alist-get 'marked-ingress next)
                                 ingress-names))
         (setf (alist-get 'ingress-pending-deletion next)
               (seq-intersection (alist-get 'ingress-pending-deletion next)
                                 ingress-names))))
      ;; Jobs

      (:mark-job
       (let ((cur (alist-get 'marked-jobs state)))
         (setf (alist-get 'marked-jobs next)
               (delete-dups (cons args cur)))))
      (:unmark-job
       (setf (alist-get 'marked-jobs next)
             (remove args (alist-get 'marked-jobs next))))
      (:delete-job
       (let ((updated (cons args (alist-get 'jobs-pending-deletion state))))
         (setf (alist-get 'jobs-pending-deletion next)
               (delete-dups updated))))
      (:update-jobs
       (setf (alist-get 'jobs next) args)

       (-let* (((&alist 'items jobs) args)
               (job-names (seq-map #'kubernetes-state-resource-name (append jobs nil))))
         (setf (alist-get 'marked-jobs next)
               (seq-intersection (alist-get 'marked-jobs next)
                                 job-names))
         (setf (alist-get 'jobs-pending-deletion next)
               (seq-intersection (alist-get 'jobs-pending-deletion next)
                                 job-names))))

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
               (deployment-names (seq-map #'kubernetes-state-resource-name (append deployments nil))))
         (setf (alist-get 'marked-deployments next)
               (seq-intersection (alist-get 'marked-deployments next)
                                 deployment-names))
         (setf (alist-get 'deployments-pending-deletion next)
               (seq-intersection (alist-get 'deployments-pending-deletion next)
                                 deployment-names))))

      ;; Statefulsets

      (:mark-statefulset
       (let ((cur (alist-get 'marked-statefulsets state)))
         (setf (alist-get 'marked-statefulsets next)
               (delete-dups (cons args cur)))))
      (:unmark-statefulset
       (setf (alist-get 'marked-statefulsets next)
             (remove args (alist-get 'marked-statefulsets next))))
      (:delete-statefulset
       (let ((updated (cons args (alist-get 'statefulsets-pending-deletion state))))
         (setf (alist-get 'statefulsets-pending-deletion next)
               (delete-dups updated))))
      (:update-statefulsets
       (setf (alist-get 'statefulsets next) args)

       ;; Prune deleted statefulsets from state.
       (-let* (((&alist 'items statefulsets) args)
               (statefulset-names (seq-map #'kubernetes-state-resource-name (append statefulsets nil))))
         (setf (alist-get 'marked-statefulsets next)
               (seq-intersection (alist-get 'marked-statefulsets next)
                                 statefulset-names))
         (setf (alist-get 'statefulsets-pending-deletion next)
               (seq-intersection (alist-get 'statefulsets-pending-deletion next)
                                 statefulset-names))))

      ;; Nodes

      (:update-nodes
       (setf (alist-get 'nodes next) args))

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

(defun kubernetes-state-mark-job (job-name)
  (cl-assert (stringp job-name))
  (kubernetes-state-update :mark-job job-name))

(defun kubernetes-state-unmark-job (job-name)
  (cl-assert (stringp job-name))
  (kubernetes-state-update :unmark-job job-name))

(defun kubernetes-state-delete-job (job-name)
  (cl-assert (stringp job-name))
  (kubernetes-state-update :delete-job job-name)
  (kubernetes-state-update :unmark-job job-name))

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

(defun kubernetes-state-mark-ingress (ingress-name)
  (cl-assert (stringp ingress-name))
  (kubernetes-state-update :mark-ingress ingress-name))

(defun kubernetes-state-unmark-ingress (ingress-name)
  (cl-assert (stringp ingress-name))
  (kubernetes-state-update :unmark-ingress ingress-name))

(defun kubernetes-state-delete-ingress (ingress-name)
  (cl-assert (stringp ingress-name))
  (kubernetes-state-update :delete-ingress ingress-name)
  (kubernetes-state-update :unmark-ingress ingress-name))

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

(defun kubernetes-state-mark-statefulset (statefulset-name)
  (cl-assert (stringp statefulset-name))
  (kubernetes-state-update :mark-statefulset statefulset-name))

(defun kubernetes-state-unmark-statefulset (statefulset-name)
  (cl-assert (stringp statefulset-name))
  (kubernetes-state-update :unmark-statefulset statefulset-name))

(defun kubernetes-state-delete-statefulset (statefulset-name)
  (cl-assert (stringp statefulset-name))
  (kubernetes-state-update :delete-statefulset statefulset-name)
  (kubernetes-state-update :unmark-statefulset statefulset-name))

(defun kubernetes-state-mark-persistentvolumeclaim (persistentvolumeclaim-name)
  (cl-assert (stringp persistentvolumeclaim-name))
  (kubernetes-state-update :mark-persistentvolumeclaim persistentvolumeclaim-name))

(defun kubernetes-state-unmark-persistentvolumeclaim (persistentvolumeclaim-name)
  (cl-assert (stringp persistentvolumeclaim-name))
  (kubernetes-state-update :unmark-persistentvolumeclaim persistentvolumeclaim-name))

(defun kubernetes-state-delete-persistentvolumeclaim (persistentvolumeclaim-name)
  (cl-assert (stringp persistentvolumeclaim-name))
  (kubernetes-state-update :delete-persistentvolumeclaim persistentvolumeclaim-name)
  (kubernetes-state-update :unmark-persistentvolumeclaim persistentvolumeclaim-name))

(defun kubernetes-state-unmark-all ()
  (kubernetes-state-update :unmark-all))


(defun kubernetes-state--refresh-now (type &optional interactive raw)
  "Force a refresh of the state data for the given resource TYPE.

INTERACTIVE denotes whether or not this function was invoked
interactively.  RAW allows for providing explicit kubectl
arguments."
  (interactive "SResource: \np\ni")
  (kubernetes-kubectl-await
   (apply-partially #'kubernetes-kubectl
                    (kubernetes-state)
                    (if raw (split-string raw) (list "get" (symbol-name type) "-o" "json")))
   (lambda (buf)
     (with-current-buffer buf
       (when interactive
         (message (concat "Updated " (symbol-name type) ".")))
       (funcall (intern (format "kubernetes-state-update-%s" (symbol-name type)))
                (json-read-from-string (buffer-string)))
       (-let* (((&alist 'items)
                (kubernetes-state--get (kubernetes-state) type)))
         (seq-map (lambda (item)
                    (-let* (((&alist 'metadata (&alist 'name)) item)) name))
                  items))))
   nil
   #'ignore))


;; State accessors

(defmacro kubernetes-state-define-refreshers (attr &optional canned raw)
  (declare (indent 2))
  (let* ((s-attr (symbol-name attr))
         (canned (or canned (-partial #'kubernetes-kubectl-get s-attr))))
    `(progn
       (defun ,(intern (format "kubernetes-%s-refresh" s-attr)) (&optional interactive)
         (unless (poll-process-live-p kubernetes--global-process-ledger (quote ,attr))
           (set-process-for-resource kubernetes--global-process-ledger (quote ,attr)
            (funcall
             ,canned
             (kubernetes-state)
             (lambda (response)
               (,(intern (format "kubernetes-state-update-%s" s-attr)) response)
               (when interactive
                 (message (concat "Updated " ,s-attr "."))))
             (-partial 'release-process-for-resource kubernetes--global-process-ledger (quote ,attr))
             ))))
       (defun ,(intern (format "kubernetes-%s-refresh-now" s-attr)) (&optional interactive)
         (interactive "p")
         (kubernetes-state--refresh-now (quote ,attr) interactive ,raw)))))

(defun kubernetes-state--get (state type)
  "Get the entry for corresponding resource TYPE from STATE."
  (alist-get type state))

(defmacro kubernetes-state--define-setter (attr arglist &rest forms-before-update)
  (declare (indent 2))
  (let ((arg
         (pcase arglist
           (`(,x) x)
           (xs `(list ,@xs)))))
    `(defun ,(intern (format "kubernetes-state-update-%s" attr)) ,arglist
       ,@forms-before-update
       (let ((prev (kubernetes-state--get (kubernetes-state) (quote ,attr)))
             (arg ,arg))
         (kubernetes-state-update ,(intern (format ":update-%s" attr)) ,arg)

         ;; Redraw immediately if this value was previously unset.
         (unless prev
           (kubernetes-state-trigger-redraw))

         arg))))

(kubernetes-state--define-setter
    current-namespace
    (namespace)
  (cl-assert (stringp namespace)))

(kubernetes-state--define-setter pods (pods)
  (cl-assert (listp pods)))

(kubernetes-state--define-setter ingress (ingress)
  (cl-assert (listp ingress)))

(kubernetes-state--define-setter jobs (jobs)
  (cl-assert (listp jobs)))

(kubernetes-state--define-setter configmaps (configmaps)
  (cl-assert (listp configmaps)))

(kubernetes-state--define-setter secrets (secrets)
  (cl-assert (listp secrets)))

(kubernetes-state--define-setter services (services)
  (cl-assert (listp services)))

(kubernetes-state--define-setter deployments (deployments)
  (cl-assert (listp deployments)))

(kubernetes-state--define-setter nodes (nodes)
  (cl-assert (listp nodes)))

(kubernetes-state--define-setter statefulsets (statefulsets)
  (cl-assert (listp statefulsets)))

(kubernetes-state--define-setter namespaces (namespaces)
  (cl-assert (listp namespaces)))

(kubernetes-state--define-setter config (config)
  (cl-assert (listp config)))

(kubernetes-state--define-setter label-query (label-name)
  (cl-assert (stringp label-name)))

(kubernetes-state--define-setter persistentvolumeclaims (persistentvolumeclaims)
  (cl-assert (listp persistentvolumeclaims)))

(defun kubernetes-state-overview-sections (state)
  (or (alist-get 'overview-sections state)
      (let* ((configurations (append kubernetes-overview-custom-views-alist kubernetes-overview-views-alist))
             (sections (alist-get kubernetes-default-overview-view configurations))
             (updated (kubernetes-state-update :update-overview-sections sections)))
        (alist-get 'overview-sections updated))))

(kubernetes-state--define-setter overview-sections (resources)
  (cl-assert (--all? (member it '(context
                                  configmaps
                                  overview
                                  deployments
                                  statefulsets
                                  ingress
                                  jobs
                                  pods
                                  secrets
                                  services
                                  nodes
                                  persistentvolumeclaims))
                     resources)))

(defun kubernetes-state-kubectl-flags (state)
  (or (alist-get 'kubectl-flags state)
      (let ((updated (kubernetes-state-update :update-kubectl-flags kubernetes-kubectl-flags)))
        (alist-get 'kubectl-flags updated))))

(kubernetes-state--define-setter kubectl-flags (flags)
  (cl-assert (listp flags))
  (cl-assert (-all? #'stringp flags))
  (setq kubernetes-kubectl-flags flags))

(defun kubernetes-state-update-last-error (message command time)
  (cl-assert (stringp message))
  (cl-assert (stringp command))
  (cl-assert time)
  (cl-assert (listp time))
  (cl-assert (-all? #'integerp time))
  (let ((arg `((message . ,message)
               (command . ,command)
               (time . ,time))))
    (kubernetes-state-update :update-last-error arg)
    arg))

;; No update function is provided for time. The time is updated internally
;; before the redrawing hook is run.
(defun kubernetes-state-current-time (state)
  "Get the current time from STATE."
  (kubernetes-state--get state 'current-time))

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

(kubernetes-state-define-named-lookup configmap configmaps)
(kubernetes-state-define-named-lookup deployment deployments)
(kubernetes-state-define-named-lookup statefulset statefulsets)
(kubernetes-state-define-named-lookup ingress ingress)
(kubernetes-state-define-named-lookup job jobs)
(kubernetes-state-define-named-lookup namespace namespaces)
(kubernetes-state-define-named-lookup pod pods)
(kubernetes-state-define-named-lookup secret secrets)
(kubernetes-state-define-named-lookup service services)
(kubernetes-state-define-named-lookup node nodes)
(kubernetes-state-define-named-lookup persistentvolumeclaim persistentvolumeclaims)

(defun kubernetes-state-resource-name (resource)
  "Get the name of RESOURCE from its metadata.

RESOURCE is the parsed representation an API resource, such a
pod, secret, configmap, etc."
  (-let [(&alist 'metadata (&alist 'name name)) resource]
    name))

(defun kubernetes-state-resource-label (resource)
  "Get the label of RESOURCE from its metadata.

RESOURCE is the parsed representation an API resource, such a
pod, secret, configmap, etc."
  (-let [(&alist 'metadata (&alist 'labels (&alist 'name label))) resource]
    label))

(defun kubernetes-state-current-context (state)
  (when-let (config (kubernetes-state--get state 'config))
    (kubernetes-state--lookup-current-context config)))

(defun kubernetes-state-clear-error-if-stale (error-display-time)
  (-when-let ((&alist 'time err-time) (kubernetes-state--get (kubernetes-state) 'last-error))
    (when (< error-display-time
             (- (time-to-seconds) (time-to-seconds err-time)))
      (kubernetes-state-update :update-last-error nil))))

(defun kubernetes-state-trigger-redraw ()
  (kubernetes-state-update :update-current-time (current-time))
  (kubernetes-state-clear-error-if-stale kubernetes-minimum-error-display-time)
  (run-hooks 'kubernetes-redraw-hook))


(provide 'kubernetes-state)

;;; kubernetes-state.el ends here
