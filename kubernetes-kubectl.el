;;; kubernetes-kubectl.el --- Low-level kubectl integration routines.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 'with-editor)

(require 'kubernetes-process)
(require 'kubernetes-props)
(require 'kubernetes-state)
(require 'kubernetes-vars)

(autoload 'json-read-from-string "json")
(autoload 'kubernetes-utils-up-to-existing-dir "kubernetes-utils")

(defun kubernetes-kubectl--default-error-handler (props status)
  (unless (kubernetes-props-overview-buffer-selected-p props)
    (let* ((last-error (kubernetes-props-get-last-error props))
           (last-error (concat
                        (or (when (listp last-error)
                              (alist-get 'command last-error))
                            "undefined command")
                        ": "
                        (or (when (listp last-error)
                              (alist-get 'message last-error))
                            "undefined error")))
           (process-killed-manually (string-match-p (rx bol (* space) "killed:" (* space) "9" (* space) eol) status)))
      (unless process-killed-manually
        (kubernetes-props-message props (string-trim-right last-error))))))

(defun kubernetes-kubectl--flags-from-state (state)
  (append (when-let (ns (kubernetes-state--get state 'current-namespace))
            (list (format "--namespace=%s" ns)))
          (kubernetes-state-kubectl-flags state)))

(cl-defun kubernetes-kubectl (props
                              state
                              args
                              on-success
                              &optional on-error cleanup-cb
                              &key flags)
  "Run kubectl with ARGS.

PROPS is an alist of functions to inject.  It should normally be passed
`kubernetes-props'.

STATE is the current application state, used to apply additional
global flags to kubectl.  If FLAGS is set, this is ignored and
can safely be set to nil.

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
  (let* ((flags (or flags (kubernetes-kubectl--flags-from-state state)))
         (command (append (list kubernetes-kubectl-executable) args flags))
         (buf (generate-new-buffer (format " kubectl: %s" command)))
         (err-buf (generate-new-buffer (format " kubectl-err: %s" command)))

         ;; `default-directory' must exist, otherwise `make-process' raises an
         ;; error.
         (default-directory (kubernetes-utils-up-to-existing-dir default-directory)))

    ;; Clean up stderr buffer when stdout buffer is killed.
    (with-current-buffer buf
      (add-hook 'kill-buffer-hook (lambda ()
                                    (let ((kill-buffer-query-functions nil))
                                      (ignore-errors (kill-buffer err-buf))))
                nil t))

    (make-process
     :name (format "kubectl: %s" (s-join " " command))
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

(defun kubernetes-kubectl-get (resource props state cb &optional cleanup-cb)
  "Get all of a given RESOURCE and execute callback CB with the parsed JSON.

PROPS is an alist of functions to inject.  It should normally be passed
`kubernetes-props'.

STATE is the application state.

CLEANUP-CB is a function taking no arguments used to release any resources."
  (kubernetes-kubectl props state `("get" ,resource "-o" "json")
                      (lambda (buf)
                        (let ((json (with-current-buffer buf
                                      (json-read-from-string (buffer-string)))))
                          (funcall cb json)))
                      nil
                      cleanup-cb))

(defun kubernetes-kubectl-delete (type name props state cb &optional error-cb)
  "Delete resource of TYPE and NAME; execute CB with the response buffer.

PROPS is an alist of functions to inject.  It should normally be passed
`kubernetes-props'.

STATE is the application state.

ERROR-CB is called if an error occurred."

  (kubernetes-kubectl props state `("delete" ,type ,name "-o" "name")
                      (lambda (buf)
                        `(with-current-buffer buf
                          (string-match
                            (rx bol ,type "/" (group (+ nonl)))
                            (buffer-string))
                          (funcall cb (match-string 1 (buffer-string)))))
                      error-cb))

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

(defun kubernetes-kubectl-describe-pod (props state pod-name cb)
  "Describe pod with POD-NAME, then execute CB with the string response.

PROPS is an alist of functions to inject.  It should normally be passed
`kubernetes-props'.

STATE is the application state."
  (kubernetes-kubectl props state (list "describe" "pod" pod-name)
                      (lambda (buf)
                        (let ((s (with-current-buffer buf (buffer-string))))
                          (funcall cb s)))))

(defun kubernetes-kubectl-await (command &rest callbacks)
  "Apply COMMAND to list of CALLBACKS where first callback is assumed on-success.
If no callbacks called within `kubernetes-kubectl-timeout-seconds', give up,
possibly orphaning a process.
Return result of first callback if success, nil otherwise."
  (let* (result
         complete
         sentinelized
         (sentinel (lambda (&rest _args) (setq complete t))))
    (mapc (lambda (f)
            (when (functionp f)
              (add-function :before (var f) sentinel))
            (push f sentinelized))
          callbacks)
    (setf sentinelized (nreverse sentinelized))
    (when (functionp (car sentinelized))
      (add-function :around (car sentinelized)
                    (lambda (f &rest args)
                      (setq result (apply f args)))))
    (cl-loop initially do (apply command sentinelized)
             with ms = 500
             with count = (max 1 (truncate
                                  (/ (* 1000 kubernetes-kubectl-timeout-seconds)
                                     ms)))
             repeat count
             until complete
             do (sleep-for 0 ms)
             finally return result)))

(defmacro kubernetes-kubectl-await-command (resource for-items)
  (declare (indent defun))
  "Await kubectl updating state's RESOURCE and return result of calling
FOR-ITEMS on updated RESOURCEs."
  `(kubernetes-kubectl-await
    (apply-partially #'kubernetes-kubectl
                     kubernetes-props
                     (kubernetes-state)
                     (split-string ,(format "get %s -o json" (symbol-name resource))))
    (lambda (buf)
      (with-current-buffer buf
        (,(intern (concat "kubernetes-state-update-" (symbol-name resource)))
         (json-read-from-string (buffer-string)))
        (-let* (((&alist 'items)
                 (,(intern (concat "kubernetes-state-" (symbol-name resource)))
                  (kubernetes-state))))
          (seq-map ,for-items items))))
    nil
    #'ignore))

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

(defun kubernetes-kubectl-edit-resource (props state kind resource-name cb &optional error-cb)
  "Edit resource of kind KIND with RESOURCE-NAME, then execute CB
with the response buffer.

PROPS is an alist of functions to inject.  It should normally be passed
`kubernetes-props'.

STATE is the application state.

ERROR-CB is called if an error occurred."
  (with-editor
    (kubernetes-kubectl props
                        state
                        (list "edit" kind resource-name)
                        cb
                        error-cb)))

(provide 'kubernetes-kubectl)

;;; kubernetes-kubectl.el ends here
