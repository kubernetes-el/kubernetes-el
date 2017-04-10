;;; kubernetes-state.el --- Main state for Kubernetes  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 'subr-x)

(require 'kubernetes-custom)
(require 'kubernetes-process)


;;; Main state

(defvar kubernetes--last-error nil
  "The last error response from kubectl.

It is an alist with the following keys:
  - message : the stderr from the process
  - time : the time at which the error was set
  - command : the executable command that failed.

Used to provide error feedback in the overview.")

(defvar kubernetes--get-pods-response nil
  "State representing the get pods response from the API.

Used to draw the pods list of the main buffer.")

(defvar kubernetes--get-configmaps-response nil
  "State representing the get configmaps response from the API.")

(defvar kubernetes--get-secrets-response nil
  "State representing the get secrets response from the API.")

(defvar kubernetes--get-services-response nil
  "State representing the get services response from the API.")

(defvar kubernetes--view-config-response nil
  "State representing the view config response from the API.

Used to draw the context section of the main buffer.")

(defvar kubernetes--get-namespaces-response nil
  "State representing the namespaces response from the API.

Used for namespace selection within a cluster.")

(defvar kubernetes--current-namespace nil
  "The namespace to use in queries.  Overrides the context settings.")

(defun kubernetes--state-clear ()
  (setq kubernetes--last-error nil)
  (setq kubernetes--get-pods-response nil)
  (setq kubernetes--get-configmaps-response nil)
  (setq kubernetes--get-secrets-response nil)
  (setq kubernetes--get-services-response nil)
  (setq kubernetes--view-config-response nil)
  (setq kubernetes--get-namespaces-response nil)
  (setq kubernetes--current-namespace nil))

(defun kubernetes--state-clear-error-if-stale ()
  (-when-let ((&alist 'error (&alist 'time err-time)) (kubernetes--state))
    (when (< kubernetes-minimum-error-display-time
             (- (time-to-seconds) (time-to-seconds err-time)))
      (setq kubernetes--last-error nil))))

(defun kubernetes--state-set-error (message command)
  (setq kubernetes--last-error `((message . ,message)
                                 (time . ,(current-time))
                                 (command . ,command))))

(defun kubernetes--state ()
  "Return the current state as an alist."
  `((pods . ,kubernetes--get-pods-response)
    (error . ,kubernetes--last-error)
    (configmaps . ,kubernetes--get-configmaps-response)
    (secrets . ,kubernetes--get-secrets-response)
    (services . ,kubernetes--get-services-response)
    (config . ,kubernetes--view-config-response)
    (namespaces . ,kubernetes--get-namespaces-response)
    (current-namespace . ,kubernetes--current-namespace)
    (current-time . ,(current-time))))

(defun kubernetes--state-lookup-pod (pod-name)
  "Look up a pod by name in the current state.

POD-NAME is the name of the pod to search for.

If lookup succeeds, return the alist representation of the pod.
If lookup fails, return nil."
  (-let [(&alist 'pods (&alist 'items pods)) (kubernetes--state)]
    (--find (equal (kubernetes--resource-name it) pod-name)
            (append pods nil))))

(defun kubernetes--state-lookup-configmap (configmap-name)
  "Look up a configmap by name in the current state.

CONFIGMAP-NAME is the name of the configmap to search for.

If lookup succeeds, return the alist representation of the configmap.
If lookup fails, return nil."
  (-let [(&alist 'configmaps (&alist 'items configmaps)) (kubernetes--state)]
    (--find (equal (kubernetes--resource-name it) configmap-name)
            (append configmaps nil))))

(defun kubernetes--state-lookup-secret (secret-name)
  "Look up a secret by name in the current state.

SECRET-NAME is the name of the secret to search for.

If lookup succeeds, return the alist representation of the secret.
If lookup fails, return nil."
  (-let [(&alist 'secrets (&alist 'items secrets)) (kubernetes--state)]
    (--find (equal (kubernetes--resource-name it) secret-name)
            (append secrets nil))))

(defun kubernetes--state-lookup-service (service-name)
  "Look up a service by name in the current state.

SERVICE-NAME is the name of the service to search for.

If lookup succeeds, return the alist representation of the service.
If lookup fails, return nil."
  (-let [(&alist 'services (&alist 'items services)) (kubernetes--state)]
    (--find (equal (kubernetes--resource-name it) service-name)
            (append services nil))))

(defun kubernetes--resource-name (resource)
  "Get the name of RESOURCE from its metadata.

RESOURCE is the parsed representation an API resource, such a
pod, secret, configmap, etc."
  (-let [(&alist 'metadata (&alist 'name name)) resource]
    name))


;;; Background polling processes.

(defmacro kubernetes-define-polling-process (resource)
  "Create resource polling-related definitions.

RESOURCE is the name of the resource as a symbol.

Defines the following functions:

- `kubernetes--set-poll-RESOURCE-process'
- `kubernetes--release-poll-RESOURCE-process'
- `kubernetes--poll-RESOURCE-process'."
  (unless (symbolp resource) (error "RESOURCE must be a symbol"))
  (let ((proc-var-name (intern (format "kubernetes--internal-poll-%s-process" resource)))
        (proc-live-p (intern (format "kubernetes--poll-%s-process-live-p" resource)))
        (releaser-name (intern (format "kubernetes--release-poll-%s-process" resource)))
        (setter-name (intern (format "kubernetes--set-poll-%s-process" resource))))
    `(progn
       (defvar ,proc-var-name nil
         "Variable used to coordinate polling access to resources.

Do not use this variable directly. Instead, use its corresponding accessors.")

       (defun ,proc-live-p ()
         "Get the polling process for this resource if it is running."
         (when-let (proc ,proc-var-name)
           (when (process-live-p proc)
             proc)))

       (defun ,setter-name (proc)
         "Set the polling process to PROC."
         (,releaser-name)
         (setq ,proc-var-name proc))

       (defun ,releaser-name ()
         "Kill the existing polling process, if any."
         (kubernetes--kill-process-quietly ,proc-var-name)
         (setq ,proc-var-name nil)))))

(kubernetes-define-polling-process namespaces)
(kubernetes-define-polling-process context)
(kubernetes-define-polling-process pods)
(kubernetes-define-polling-process configmaps)
(kubernetes-define-polling-process secrets)
(kubernetes-define-polling-process services)

(defun kubernetes--kill-polling-processes ()
  (kubernetes--release-poll-namespaces-process)
  (kubernetes--release-poll-services-process)
  (kubernetes--release-poll-context-process)
  (kubernetes--release-poll-pods-process)
  (kubernetes--release-poll-configmaps-process)
  (kubernetes--release-poll-secrets-process))


;; Polling and redisplay timers

(defvar kubernetes--poll-timer nil
  "Background timer used to poll for updates.

This is used to regularly synchronise local state with Kubernetes.")

(defvar kubernetes--redraw-timer nil
  "Background timer used to trigger buffer redrawing.

This is used to display the current state.")

(defun kubernetes--initialize-timers ()
  (unless kubernetes--redraw-timer
    (setq kubernetes--redraw-timer (run-with-timer kubernetes-redraw-frequency kubernetes-redraw-frequency 'kubernetes--redraw-buffers)))
  (unless kubernetes--poll-timer
    (setq kubernetes--poll-timer (run-with-timer kubernetes-poll-frequency kubernetes-poll-frequency 'kubernetes-refresh))))

(defun kubernetes--kill-timers ()
  (when-let (timer kubernetes--redraw-timer)
    (cancel-timer timer))
  (when-let (timer kubernetes--poll-timer)
    (cancel-timer timer))
  (setq kubernetes--redraw-timer nil)
  (setq kubernetes--poll-timer nil))


(provide 'kubernetes-state)

;;; kubernetes-state.el ends here
