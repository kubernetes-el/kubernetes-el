;;; kubernetes-kubectl.el --- Low-level kubectl integration routines.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)

(require 'kubernetes-process)
(require 'kubernetes-props)
(require 'kubernetes-state)
(require 'kubernetes-vars)

(autoload 'json-read-from-string "json")
(autoload 'kubernetes-utils-up-to-existing-dir "kubernetes-utils")

(defun kubernetes-kubectl--default-error-handler (props status)
  (unless (kubernetes-props-overview-buffer-selected-p props)
    (-let* ((last-error-already-set (kubernetes-props-get-last-error props))
            (process-killed-manually (string-match-p (rx bol (* space) "killed:" (* space) "9" (* space) eol) status)))
      (unless (or process-killed-manually last-error-already-set)
        (kubernetes-props-message props "kubernetes command failed.  See the overview buffer for details.")))))

(defun kubernetes-kubectl--flags-from-state (state)
  (append (when-let (ns (kubernetes-state-current-namespace state))
            (list (format "--namespace=%s" ns)))
          (kubernetes-state-kubectl-flags state)))

(defun kubernetes-kubectl (props state args on-success &optional on-error cleanup-cb)
  "Run kubectl with ARGS.

PROPS is an alist of functions to inject.  It should normally be passed
`kubernetes-props'.

STATE is the current application state, used to apply additional
global flags to kubectl.

ON-SUCCESS is a function of one argument, called with the process' buffer.

Optional ON-ERROR is a function of two arguments, called with the
process' stderr buffer.  If omitted, it defaults to
`kubernetes-kubectl--default-error-handler', which logs an error
if the process exited unexpectedly.

Optional CLEANUP-CB is a function of no arguments that is always
called after the other callbacks.  It can be used for releasing
resources.

After callbacks are executed, the process and its buffer will be killed.

Returns the process object for this execution of kubectl."
  (let* ((buf (generate-new-buffer " kubectl"))
         (err-buf (generate-new-buffer " kubectl-err"))
         (command (append (list kubernetes-kubectl-executable) args (kubernetes-kubectl--flags-from-state state)))

         ;; `default-directory' must exist, otherwise `make-process' raises an
         ;; error.
         (default-directory (kubernetes-utils-up-to-existing-dir default-directory))
         (proc (make-process
                :name "kubectl"
                :buffer buf
                :stderr err-buf
                :command command
                :noquery t
                :sentinel
                (lambda (proc status)
                  (unwind-protect
                      (let ((exit-code (process-exit-status proc)))
                        (cond
                         ((zerop exit-code)
                          (funcall on-success buf))
                         (t
                          (let ((err-message (with-current-buffer err-buf (buffer-string))))
                            (unless (= 9 exit-code)
                              (kubernetes-props-update-last-error props err-message (string-join command " ") (current-time))))
                          (cond (on-error
                                 (funcall on-error err-buf))
                                (t
                                 (kubernetes-kubectl--default-error-handler props status))))))
                    (when cleanup-cb
                      (funcall cleanup-cb))
                    (kubernetes-process-kill-quietly proc))))))

    ;; Clean up stderr buffer when stdout buffer is killed.
    (with-current-buffer buf
      (add-hook 'kill-buffer-hook (lambda ()
                                    (let ((kill-buffer-query-functions nil))
                                      (ignore-errors (kill-buffer err-buf))))
                nil t))

    proc))

(defun kubernetes-kubectl-get-pods (props state cb &optional cleanup-cb)
  "Get all pods and execute callback CB with the parsed JSON.

PROPS is an alist of functions to inject.  It should normally be passed
`kubernetes-props'.

STATE is the application state.

CLEANUP-CB is a function taking no arguments used to release any resources."
  (kubernetes-kubectl props state '("get" "pods" "-o" "json")
                      (lambda (buf)
                        (let ((json (with-current-buffer buf
                                      (json-read-from-string (buffer-string)))))
                          (funcall cb json)))
                      nil
                      cleanup-cb))

(defun kubernetes-kubectl-get-configmaps (props state cb &optional cleanup-cb)
  "Get all configmaps and execute callback CB with the parsed JSON.

PROPS is an alist of functions to inject.  It should normally be passed
`kubernetes-props'.

STATE is the application state.

CLEANUP-CB is a function taking no arguments used to release any resources."
  (kubernetes-kubectl props state '("get" "configmaps" "-o" "json")
                      (lambda (buf)
                        (let ((json (with-current-buffer buf
                                      (json-read-from-string (buffer-string)))))
                          (funcall cb json)))
                      nil
                      cleanup-cb))

(defun kubernetes-kubectl-get-deployments (props state cb &optional cleanup-cb)
  "Get all deployments and execute callback CB with the parsed JSON.

PROPS is an alist of functions to inject.  It should normally be passed
`kubernetes-props'.

STATE is the application state.

CLEANUP-CB is a function taking no arguments used to release any resources."
  (kubernetes-kubectl props state '("get" "deployments" "-o" "json")
                      (lambda (buf)
                        (let ((json (with-current-buffer buf
                                      (json-read-from-string (buffer-string)))))
                          (funcall cb json)))
                      nil
                      cleanup-cb))

(defun kubernetes-kubectl-get-jobs (props state cb &optional cleanup-cb)
  "Get all jobs and execute callback CB with the parsed JSON.

PROPS is an alist of functions to inject.  It should normally be passed
`kubernetes-props'.

STATE is the application state.

CLEANUP-CB is a function taking no arguments used to release any resources."
  (kubernetes-kubectl props state '("get" "jobs" "-o" "json")
                      (lambda (buf)
                        (let ((json (with-current-buffer buf
                                      (json-read-from-string (buffer-string)))))
                          (funcall cb json)))
                      nil
                      cleanup-cb))

(defun kubernetes-kubectl-get-secrets (props state cb &optional cleanup-cb)
  "Get all secrets and execute callback CB with the parsed JSON.

PROPS is an alist of functions to inject.  It should normally be passed
`kubernetes-props'.

STATE is the application state.

CLEANUP-CB is a function taking no arguments used to release any resources."
  (kubernetes-kubectl props state '("get" "secrets" "-o" "json")
                      (lambda (buf)
                        (let ((json (with-current-buffer buf
                                      (json-read-from-string (buffer-string)))))
                          (funcall cb json)))
                      nil
                      cleanup-cb))

(defun kubernetes-kubectl-get-services (props state cb &optional cleanup-cb)
  "Get all services and execute callback CB with the parsed JSON.

PROPS is an alist of functions to inject.  It should normally be passed
`kubernetes-props'.

STATE is the application state.

CLEANUP-CB is a function taking no arguments used to release any resources."
  (kubernetes-kubectl props state '("get" "services" "-o" "json")
                      (lambda (buf)
                        (let ((json (with-current-buffer buf
                                      (json-read-from-string (buffer-string)))))
                          (funcall cb json)))
                      nil
                      cleanup-cb))

(defun kubernetes-kubectl-config-view (props state cb &optional cleanup-cb)
  "Get the current configuration and pass it to CB.

PROPS is an alist of functions to inject.  It should normally be passed
`kubernetes-props'.

STATE is the application state.

CLEANUP-CB is a function taking no arguments used to release any resources."
  (kubernetes-kubectl props
                      state
                      '("config" "view" "-o" "json")
                      (lambda (buf)
                        (let ((json (with-current-buffer buf
                                      (json-read-from-string (buffer-string)))))
                          (funcall cb json)))
                      nil
                      cleanup-cb))

(defun kubernetes-kubectl-config-use-context (props state context-name cb)
  "Change the current kubernetes context to CONTEXT-NAME, a string.

PROPS is an alist of functions to inject.  It should normally be passed
`kubernetes-props'.

STATE is the application state.

CB is a function taking the name of the context that was switched to."
  (kubernetes-kubectl props
                      state
                      (list "config" "use-context" context-name)
                      (lambda (buf)
                        (with-current-buffer buf
                          (string-match (rx bol "Switched to context \"" (group (+? nonl)) "\"." (* space) eol)
                                        (buffer-string))
                          (funcall cb (match-string 1 (buffer-string)))))))

(defun kubernetes-kubectl-get-namespaces (props state cb &optional cleanup-cb)
  "Get namespaces for the current cluster and pass the parsed response to CB.

PROPS is an alist of functions to inject.  It should normally be passed
`kubernetes-props'.

STATE is the application state.

CLEANUP-CB is a function taking no arguments used to release any resources."
  (kubernetes-kubectl props state '("get" "namespaces" "-o" "json")
                      (lambda (buf)
                        (let ((json (with-current-buffer buf
                                      (json-read-from-string (buffer-string)))))
                          (funcall cb json)))
                      nil
                      cleanup-cb))

(defun kubernetes-kubectl-delete-pod (props state pod-name cb &optional error-cb)
  "Delete pod with POD-NAME, then execute CB with the response buffer.

PROPS is an alist of functions to inject.  It should normally be passed
`kubernetes-props'.

STATE is the application state.

ERROR-CB is called if an error occurred."
  (kubernetes-kubectl props state (list "delete" "pod" pod-name "-o" "name")
                      (lambda (buf)
                        (with-current-buffer buf
                          (string-match (rx bol "pod/" (group (+ nonl))) (buffer-string))
                          (funcall cb (match-string 1 (buffer-string)))))
                      error-cb))

(defun kubernetes-kubectl-delete-configmap (props state configmap-name cb &optional error-cb)
  "Delete CONFIGMAP-NAME, then execute CB with the response buffer.

PROPS is an alist of functions to inject.  It should normally be passed
`kubernetes-props'.

STATE is the application state.

ERROR-CB is called if an error occurred."
  (kubernetes-kubectl props state (list "delete" "configmap" configmap-name "-o" "name")
                      (lambda (buf)
                        (with-current-buffer buf
                          (string-match (rx bol "configmap/" (group (+ nonl))) (buffer-string))
                          (funcall cb (match-string 1 (buffer-string)))))
                      error-cb))

(defun kubernetes-kubectl-delete-secret (props state secret-name cb &optional error-cb)
  "Delete SECRET-NAME, then execute CB with the response buffer.

PROPS is an alist of functions to inject.  It should normally be passed
`kubernetes-props'.

STATE is the application state.

ERROR-CB is called if an error occurred."
  (kubernetes-kubectl props state (list "delete" "secret" secret-name "-o" "name")
                      (lambda (buf)
                        (with-current-buffer buf
                          (string-match (rx bol "secret/" (group (+ nonl))) (buffer-string))
                          (funcall cb (match-string 1 (buffer-string)))))
                      error-cb))

(defun kubernetes-kubectl-describe-pod (props state pod-name cb)
  "Describe pod with POD-NAME, then execute CB with the string response.

PROPS is an alist of functions to inject.  It should normally be passed
`kubernetes-props'.

STATE is the application state."
  (kubernetes-kubectl props state (list "describe" "pod" pod-name)
                      (lambda (buf)
                        (let ((s (with-current-buffer buf (buffer-string))))
                          (funcall cb s)))))

(defun kubernetes-kubectl-delete-service (props state service-name cb &optional error-cb)
  "Delete SERVICE-NAME, then execute CB with the response buffer.

PROPS is an alist of functions to inject.  It should normally be passed
`kubernetes-props'.

STATE is the application state.

ERROR-CB is called if an error occurred."
  (kubernetes-kubectl props state (list "delete" "service" service-name "-o" "name")
                      (lambda (buf)
                        (with-current-buffer buf
                          (string-match (rx bol "service/" (group (+ nonl))) (buffer-string))
                          (funcall cb (match-string 1 (buffer-string)))))
                      error-cb))

(defun kubernetes-kubectl-delete-deployment (props state deployment-name cb &optional error-cb)
  "Delete DEPLOYMENT-NAME, then execute CB with the response buffer.

PROPS is an alist of functions to inject.  It should normally be passed
`kubernetes-props'.

STATE is the application state.

ERROR-CB is called if an error occurred."
  (kubernetes-kubectl props state (list "delete" "deployment" deployment-name "-o" "name")
                      (lambda (buf)
                        (with-current-buffer buf
                          (string-match (rx bol "deployment/" (group (+ nonl))) (buffer-string))
                          (funcall cb (match-string 1 (buffer-string)))))
                      error-cb))

(defun kubernetes-kubectl-delete-job (props state job-name cb &optional error-cb)
  "Delete JOB-NAME, then execute CB with the response buffer.

PROPS is an alist of functions to inject.  It should normally be passed
`kubernetes-props'.

STATE is the application state.

ERROR-CB is called if an error occurred."
  (kubernetes-kubectl props state (list "delete" "job" job-name "-o" "name")
                      (lambda (buf)
                        (with-current-buffer buf
                          (string-match (rx bol "job/" (group (+ nonl))) (buffer-string))
                          (funcall cb (match-string 1 (buffer-string)))))
                      error-cb))

(defun kubernetes-kubectl-await-on-async (props state fn)
  "Turn an async function requiring a callback into a synchronous one.

PROPS is an alist of functions to inject.  It should normally be passed
`kubernetes-props'.

STATE is the application state.

Transforms a function of type:

  FN : (props, state, a -> b) -> process

to a function of the type:

  FN' : () -> a"
  (let* (complete result)
    (funcall fn props state (lambda (response)
                              (setq complete t)
                              (setq result response)))

    (while (not complete)
      (sleep-for 0.001))

    result))

(provide 'kubernetes-kubectl)

;;; kubernetes-kubectl.el ends here
