;;; kubernetes.el --- Emacs porcelain for Kubernetes.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;; Version: 0.0.1

;; Package-Requires: ((emacs "25.1") (dash "2.12.0") (magit "2.8.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'compile)
(require 'dash)
(require 'magit)
(require 'subr-x)

(autoload 'json-pretty-print-buffer "json")
(autoload 'json-read-from-string "json")
(autoload 'org-read-date "org")

(defgroup kubernetes nil
  "Emacs porcelain for Kubernetes."
  :group 'tools
  :prefix "kubernetes-")

(defcustom kubernetes-kubectl-executable "kubectl"
  "The kubectl command used for Kubernetes commands."
  :group 'kubernetes
  :type 'string)

(defcustom kubernetes-display-buffer-select t
  "Whether to select Kubernetes buffers automatically."
  :group 'kubernetes
  :type 'boolean)

(defcustom kubernetes-display-buffer-function #'kubernetes-display-buffer-fullframe
  "The function used display a Kubernetes buffer.

The function must take a single argument, which is the buffer to display."
  :group 'kubernetes
  :type '(radio (function-item kubernetes-display-buffer-fullframe)
                (function-item display-buffer)
                (function :tag "Function")))

(defcustom kubernetes-pod-restart-warning-threshold 5
  "The threshold for pod restarts above which a pod is highlighted."
  :group 'kubernetes
  :type 'number)

(defcustom kubernetes-yaml-indentation-width 2
  "The size of each indentation step in YAML.  Used by the YAML formatter."
  :group 'kubernetes
  :type 'integer)

(defcustom kubernetes-yaml-string-drop-threshold 60
  "The threshold above which a string value will be dropped to the next line."
  :group 'kubernetes
  :type 'integer)

(defcustom kubernetes-poll-frequency 5
  "The frequency at which to poll Kubernetes for changes."
  :group 'kubernetes
  :type 'integer)

(defcustom kubernetes-redraw-frequency 5
  "The buffer redraw frequency in seconds.

This is the frequency at which Kubernetes buffers will be redrawn
to match the current state.  This variable should be tuned to
balance interface stuttering with update frequency."
  :group 'kubernetes
  :type 'integer)

(defcustom kubernetes-json-mode 'javascript-mode
  "The mode to use when rendering pretty-printed JSON."
  :group 'kubernetes
  :type 'function)

(defcustom kubernetes-default-exec-command "bash"
  "The default command to use when exec'ing into a pod's container."
  :group 'kubernetes
  :type 'string)

(defcustom kubernetes-clean-up-interactive-exec-buffers t
  "If non-nil, automatically kill interactive exec buffers on process exit."
  :group 'kubernetes
  :type 'boolean)

(defface kubernetes-context-name
  '((((class color) (background light)) :foreground "SkyBlue4")
    (((class color) (background  dark)) :foreground "LightSkyBlue1"))
  "Face for context names in report buffers."
  :group 'kubernetes)

(defface kubernetes-json-key
  '((((class color) (background light)) :foreground "grey30" :weight bold)
    (((class color) (background  dark)) :foreground "grey80" :weight bold))
  "Face for keys in pretty-printed parsed JSON."
  :group 'kubernetes)

(defface kubernetes-progress-indicator
  '((t :inherit shadow))
  "Face for progress indicators."
  :group 'kubernetes)

(defface kubernetes-pending-deletion
  '((t :inherit shadow :strike-through t))
  "Face for pods awaiting deletion."
  :group 'kubernetes)

(defface kubernetes-delete-mark
  '((t :inherit error))
  "Face for deletion mark indicators."
  :group 'kubernetes)

(defconst kubernetes-display-pods-buffer-name "*kubernetes pods*")

(defconst kubernetes-display-config-buffer-name "*kubernetes config*")

(defconst kubernetes-display-configmap-buffer-name "*kubernetes configmap*")

(defconst kubernetes-display-service-buffer-name "*kubernetes service*")

(defconst kubernetes-display-configmaps-buffer-name "*kubernetes configmaps*")

(defconst kubernetes-display-secret-buffer-name "*kubernetes secret*")

(defconst kubernetes-display-secrets-buffer-name "*kubernetes secrets*")

(defconst kubernetes-overview-buffer-name "*kubernetes overview*")

(defconst kubernetes-log-line-buffer-name "*log line*")

(defconst kubernetes-logs-buffer-name "*kubernetes logs*")

(defconst kubernetes-pod-buffer-name "*kubernetes pod*")


;; Main state
;;
;; This state is cleared whenever the buffer is deleted or the context is
;; switched.

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
  (setq kubernetes--get-pods-response nil)
  (setq kubernetes--get-configmaps-response nil)
  (setq kubernetes--get-secrets-response nil)
  (setq kubernetes--get-services-response nil)
  (setq kubernetes--view-config-response nil)
  (setq kubernetes--get-namespaces-response nil)
  (setq kubernetes--current-namespace nil))

(defun kubernetes--state ()
  "Return the current state as an alist."
  `((pods . ,kubernetes--get-pods-response)
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


;; Main Kubernetes query routines

(defun kubernetes--kubectl-default-error-handler (buf status)
  (with-current-buffer buf
    (unless (string-match-p (rx bol (* space) "killed:" (* space) "9" (* space) eol) status)
      (error "Kubectl failed.  Reason: %s" (buffer-string)))))

(defun kubernetes--kubectl (args on-success &optional on-error cleanup-cb)
  "Run kubectl with ARGS.

ON-SUCCESS is a function of one argument, called with the process' buffer.

Optional ON-ERROR is a function of two argument, called with the
process' buffer.  If omitted, it defaults to
`kubernetes--kubectl-default-error-handler', which raises an
error if the process exited unexpectedly.

Optional CLEANUP-CB is a function of no arguments that is always
called after the other callbacks.  It can be used for releasing
resources.

Returns the process object for this execution of kubectl."
  (let* ((buf (generate-new-buffer " kubectl"))
         (process (apply #'start-process "kubectl" buf kubernetes-kubectl-executable args))
         (sentinel
          (lambda (proc status)
            (unwind-protect
                (cond
                 ((zerop (process-exit-status proc))
                  (funcall on-success buf))
                 (t
                  (cond (on-error
                         (message "Kubectl failed.  Reason: %s"
                                  (with-current-buffer buf
                                    (buffer-string)))
                         (funcall on-error buf))

                        (t
                         (kubernetes--kubectl-default-error-handler (process-buffer proc) status)))))
              (when cleanup-cb
                (funcall cleanup-cb))))))
    (set-process-sentinel process sentinel)
    process))

(defun kubernetes--kubectl-get-pods (cb &optional cleanup-cb)
  "Get all pods and execute callback CB with the parsed JSON.

CLEANUP-CB is a function taking no arguments used to release any resources."
  (let ((args (append '("get" "pods" "-o" "json")
                      (when kubernetes--current-namespace
                        (list (format "--namespace=%s" kubernetes--current-namespace))))))
    (kubernetes--kubectl args
               (lambda (buf)
                 (let ((json (with-current-buffer buf
                               ;; Skip past stderr written to this buffer.
                               (goto-char (point-min))
                               (search-forward "No resources found." (line-end-position) t)

                               (json-read-from-string
                                (buffer-substring (point) (point-max))))))
                   (funcall cb json)))
               nil
               cleanup-cb)))

(defun kubernetes--kubectl-get-configmaps (cb &optional cleanup-cb)
  "Get all configmaps and execute callback CB with the parsed JSON.

CLEANUP-CB is a function taking no arguments used to release any resources."
  (let ((args (append '("get" "configmaps" "-o" "json")
                      (when kubernetes--current-namespace
                        (list (format "--namespace=%s" kubernetes--current-namespace))))))
    (kubernetes--kubectl args
               (lambda (buf)
                 (let ((json (with-current-buffer buf
                               ;; Skip past stderr written to this buffer.
                               (goto-char (point-min))
                               (search-forward "No resources found." (line-end-position) t)

                               (json-read-from-string
                                (buffer-substring (point) (point-max))))))
                   (funcall cb json)))
               nil
               cleanup-cb)))

(defun kubernetes--kubectl-get-secrets (cb &optional cleanup-cb)
  "Get all secrets and execute callback CB with the parsed JSON.

CLEANUP-CB is a function taking no arguments used to release any resources."
  (let ((args (append '("get" "secrets" "-o" "json")
                      (when kubernetes--current-namespace
                        (list (format "--namespace=%s" kubernetes--current-namespace))))))
    (kubernetes--kubectl args
               (lambda (buf)
                 (let ((json (with-current-buffer buf
                               ;; Skip past stderr written to this buffer.
                               (goto-char (point-min))
                               (search-forward "No resources found." (line-end-position) t)

                               (json-read-from-string
                                (buffer-substring (point) (point-max))))))
                   (funcall cb json)))
               nil
               cleanup-cb)))

(defun kubernetes--kubectl-get-services (cb &optional cleanup-cb)
  "Get all services and execute callback CB with the parsed JSON.

CLEANUP-CB is a function taking no arguments used to release any resources."
  (let ((args (append '("get" "services" "-o" "json")
                      (when kubernetes--current-namespace
                        (list (format "--namespace=%s" kubernetes--current-namespace))))))
    (kubernetes--kubectl args
               (lambda (buf)
                 (let ((json (with-current-buffer buf
                               ;; Skip past stderr written to this buffer.
                               (goto-char (point-min))
                               (search-forward "No resources found." (line-end-position) t)

                               (json-read-from-string
                                (buffer-substring (point) (point-max))))))
                   (funcall cb json)))
               nil
               cleanup-cb)))

(defun kubernetes--kubectl-config-view (cb &optional cleanup-cb)
  "Get the current configuration and pass it to CB.

CLEANUP-CB is a function taking no arguments used to release any resources."
  (kubernetes--kubectl '("config" "view" "-o" "json")
             (lambda (buf)
               (let ((json (with-current-buffer buf
                             (json-read-from-string (buffer-string)))))
                 (funcall cb json)))
             nil
             cleanup-cb))

(defun kubernetes--kubectl-config-use-context (context-name cb)
  "Change the current kubernetes context to CONTEXT-NAME, a string.

CB is a function taking the name of the context that was switched to."
  (kubernetes--kubectl (list "config" "use-context" context-name)
             (lambda (buf)
               (with-current-buffer buf
                 (string-match (rx bol "Switched to context \"" (group (+? nonl)) "\"." (* space) eol)
                               (buffer-string))
                 (funcall cb (match-string 1 (buffer-string)))))))

(defun kubernetes--kubectl-get-namespaces (cb &optional cleanup-cb)
  "Get namespaces for the current cluster and pass the parsed response to CB.

CLEANUP-CB is a function taking no arguments used to release any resources."
  (kubernetes--kubectl '("get" "namespaces" "-o" "json")
             (lambda (buf)
               (let ((json (with-current-buffer buf
                             (json-read-from-string (buffer-string)))))
                 (funcall cb json)))
             nil
             cleanup-cb))

(defun kubernetes--kubectl-delete-pod (pod-name cb &optional error-cb)
  "Delete pod with POD-NAME, then execute CB with the response buffer.

ERROR-CB is called if an error occurred."
  (let ((args (append (list "delete" "pod" pod-name "-o" "name")
                      (when kubernetes--current-namespace
                        (list (format "--namespace=%s" kubernetes--current-namespace))))))
    (kubernetes--kubectl args
               (lambda (buf)
                 (with-current-buffer buf
                   (string-match (rx bol "pod/" (group (+ nonl))) (buffer-string))
                   (funcall cb (match-string 1 (buffer-string)))))
               error-cb)))

(defun kubernetes--kubectl-delete-configmap (configmap-name cb &optional error-cb)
  "Delete CONFIGMAP-NAME, then execute CB with the response buffer.

ERROR-CB is called if an error occurred."
  (let ((args (append (list "delete" "configmap" configmap-name "-o" "name")
                      (when kubernetes--current-namespace
                        (list (format "--namespace=%s" kubernetes--current-namespace))))))
    (kubernetes--kubectl args
               (lambda (buf)
                 (with-current-buffer buf
                   (string-match (rx bol "configmap/" (group (+ nonl))) (buffer-string))
                   (funcall cb (match-string 1 (buffer-string)))))
               error-cb)))

(defun kubernetes--kubectl-delete-secret (secret-name cb &optional error-cb)
  "Delete SECRET-NAME, then execute CB with the response buffer.

ERROR-CB is called if an error occurred."
  (let ((args (append (list "delete" "secret" secret-name "-o" "name")
                      (when kubernetes--current-namespace
                        (list (format "--namespace=%s" kubernetes--current-namespace))))))
    (kubernetes--kubectl args
               (lambda (buf)
                 (with-current-buffer buf
                   (string-match (rx bol "secret/" (group (+ nonl))) (buffer-string))
                   (funcall cb (match-string 1 (buffer-string)))))
               error-cb)))

(defun kubernetes--kubectl-describe-pod (pod-name cb)
  "Describe pod with POD-NAME, then execute CB with the string response."
  (let ((args (append (list "describe" "pod" pod-name)
                      (when kubernetes--current-namespace
                        (list (format "--namespace=%s" kubernetes--current-namespace))))))
    (kubernetes--kubectl args
               (lambda (buf)
                 (let ((s (with-current-buffer buf (buffer-string))))
                   (funcall cb s))))))

(defun kubernetes--await-on-async (fn)
  "Turn an async function requiring a callback into a synchronous one.

Transforms a function of type:

  FN : (a -> b) -> process

to a function of the type:

  FN' : () -> a"
  (let* (complete result)
    (funcall fn (lambda (response)
                  (setq complete t)
                  (setq result response)))

    (while (not complete)
      (sleep-for 0.001))

    result))


;; Utilities

(defun kubernetes--resource-name (resource)
  "Get the name of RESOURCE from its metadata.

RESOURCE is the parsed representation an API resource, such a
pod, secret, configmap, etc."
  (-let [(&alist 'metadata (&alist 'name name)) resource]
    name))

(defun kubernetes--read-pod-name ()
  "Read a pod name from the user.

Update the pod state if it not set yet."
  (-let* (((&alist 'items pods)
           (or kubernetes--get-pods-response
               (progn
                 (message "Getting pods...")
                 (let ((response (kubernetes--await-on-async #'kubernetes--kubectl-get-pods)))
                   (setq kubernetes--get-pods-response response)
                   response))))
          (pods (append pods nil))
          (names (-map #'kubernetes--resource-name pods)))
    (completing-read "Pod: " names nil t)))

(defun kubernetes--read-configmap-name ()
  "Read a configmap name from the user.

Update the configmap state if it not set yet."
  (-let* (((&alist 'items configmaps)
           (or kubernetes--get-configmaps-response
               (progn
                 (message "Getting configmaps...")
                 (let ((response (kubernetes--await-on-async #'kubernetes--kubectl-get-configmaps)))
                   (setq kubernetes--get-configmaps-response response)
                   response))))
          (configmaps (append configmaps nil))
          (names (-map #'kubernetes--resource-name configmaps)))
    (completing-read "Configmap: " names nil t)))

(defun kubernetes--read-secret-name ()
  "Read a secret name from the user.

Update the secret state if it not set yet."
  (-let* (((&alist 'items secrets)
           (or kubernetes--get-secrets-response
               (progn
                 (message "Getting secrets...")
                 (let ((response (kubernetes--await-on-async #'kubernetes--kubectl-get-secrets)))
                   (setq kubernetes--get-secrets-response response)
                   response))))
          (secrets (append secrets nil))
          (names (-map #'kubernetes--resource-name secrets)))
    (completing-read "Secret: " names nil t)))

(defun kubernetes--read-service-name ()
  "Read a service name from the user.

Update the service state if it not set yet."
  (-let* (((&alist 'items services)
           (or kubernetes--get-services-response
               (progn
                 (message "Getting services...")
                 (let ((response (kubernetes--await-on-async #'kubernetes--kubectl-get-services)))
                   (setq kubernetes--get-services-response response)
                   response))))
          (services (append services nil))
          (names (-map #'kubernetes--resource-name services)))
    (completing-read "Service: " names nil t)))

(defun kubernetes--read-iso-datetime (&rest _)
  (let* ((date (org-read-date nil t))
         (tz (format-time-string "%z" date)))
    (concat
     (format-time-string "%Y-%m-%dT%H:%M:%S" date)
     (replace-regexp-in-string (rx (group (? (any "+-")) digit digit)
                                   (group digit digit))
                               "\\1:\\2"
                               tz))))

(defun kubernetes--read-time-value (&rest _)
  "Read a relative time value in the style accepted by kubectl.  E.g. 20s, 3h, 5m."
  (let (result)
    (while (null result)
      (let ((input (read-string "Time value (e.g. 20s): ")))
        (if (string-match-p (rx bol (* space) (+ digit) (* space) (any "smh") (* space) eol)
                            input)
            (setq result input)
          (message "Invalid time value")
          (sit-for 1))))
    result))

(defun kubernetes--maybe-pod-name-at-point ()
  (pcase (get-text-property (point) 'kubernetes-nav)
    (`(:pod-name ,value)
     value)))

(defun kubernetes--json-to-yaml (json &optional level)
  "Process some parsed JSON and pretty-print as YAML.

JSON is a parsed JSON value.

LEVEL indentation level to use.  It defaults to 0 if not supplied."
  (let* ((level (or level 0))
         (space (string-to-char " "))
         (indentation (make-string (* level kubernetes-yaml-indentation-width) space))
         (body
          (cond
           ((vectorp json)
            (let* ((list-items (--map (string-trim-left (kubernetes--json-to-yaml it (1+ level)))
                                      (append json nil)))
                   (separator (concat "\n"
                                      indentation "-" "\n"
                                      indentation "  "))
                   (joined (string-join list-items separator)))
              ;; If this is an empty or singleton list, do not drop.
              (if (<= (length list-items) 1)
                  (concat indentation "- " (string-trim-right joined))
                (concat indentation "- \n"
                        indentation "  " (string-trim-right joined)))))
           ((listp json)
            (let ((entries (--map
                            (-let [(k . v) it]
                              (concat indentation
                                      (propertize (format "%s: " (symbol-name k)) 'face 'kubernetes-json-key)
                                      (cond
                                       ((equal t v) "true")
                                       ((equal :json-false v) "false")

                                       ((numberp v)
                                        (number-to-string v))

                                       ((and (stringp v) (string-match-p "\n" v))
                                        (let* ((next-indentation (make-string (* (1+ level) kubernetes-yaml-indentation-width) space))
                                               (indented
                                                (string-join
                                                 (--map (concat next-indentation it) (split-string v "\n"))
                                                 "\n")))
                                          (concat "|-\n" indented)))

                                       ((and (stringp v) (< (length v) kubernetes-yaml-string-drop-threshold))
                                        v)

                                       (t
                                        (concat "\n" (kubernetes--json-to-yaml v (1+ level)))))))
                            json)))
              (string-join entries "\n")))
           (t
            (format "%s%s" indentation json)))))
    (if (= 0 level)
        (concat (propertize "---\n" 'face 'magit-dimmed) body)
      body)))

(defun kubernetes--ellipsize (s threshold)
  (if (> (length s) threshold)
      (concat (substring s 0 (1- threshold)) "…")
    s))

(defun kubernetes--parse-utc-timestamp (timestamp)
  "Parse TIMESTAMP string from the API into the representation used by Emacs."
  (let ((parsed (parse-time-string (replace-regexp-in-string "Z" "" (replace-regexp-in-string "T" " " timestamp)))))
    (setf (nth 8 parsed) 0)
    parsed))

(defun kubernetes--time-diff-string (start now)
  "Find the interval between START and NOW, and return a string of the coarsest unit."
  (let ((diff (time-to-seconds (time-subtract now start))))
    (car (split-string (format-seconds "%yy,%dd,%hh,%mm,%ss%z" diff) ","))))

(defun kubernetes--kill-compilation-buffer (proc-buf &rest _)
  (if-let (win (get-buffer-window proc-buf))
      (quit-window t win)
    (kill-buffer proc-buf)))

(defun kubernetes--make-cleanup-fn (buf)
  "Make a function to add to `kill-buffer-hook' for a Kubernetes buffer.

BUF is the buffer used to display a Kubernetes feature.  A
reference to it is needed to determine which buffers remain.

The function will terminate polling when the last Kubernetes
buffer is killed."
  (lambda ()
    (let* ((bufs (-keep #'get-buffer (list kubernetes-display-pods-buffer-name
                                           kubernetes-display-configmaps-buffer-name
                                           kubernetes-display-secrets-buffer-name
                                           kubernetes-overview-buffer-name)))
           (more-buffers (remove buf bufs)))
      (unless more-buffers
        (dolist (b bufs)
          (with-current-buffer b
            (kubernetes--state-clear)))
        (kubernetes--kill-polling-processes)
        (kubernetes--kill-timers)))))

(defun kubernetes-display-buffer-fullframe (buffer)
  (let ((display-fn
         (lambda (buffer alist)
           (when-let (window (or (display-buffer-reuse-window buffer alist)
                                 (display-buffer-same-window buffer alist)
                                 (display-buffer-pop-up-window buffer alist)
                                 (display-buffer-use-some-window buffer alist)))
             (delete-other-windows window)
             window))))
    (display-buffer buffer (list display-fn))))

(defun kubernetes-display-buffer (buffer)
  (let ((window (funcall kubernetes-display-buffer-function buffer)))
    (when kubernetes-display-buffer-select
      (select-frame-set-input-focus
       (window-frame (select-window window))))))


;; Background polling processes

(defvar kubernetes--poll-namespaces-process nil
  "Single process used to prevent concurrent namespace refreshes.")

(defun kubernetes--set-poll-namespaces-process (proc)
  (kubernetes--release-poll-namespaces-process)
  (setq kubernetes--poll-namespaces-process proc))

(defun kubernetes--release-poll-namespaces-process ()
  (when-let (proc kubernetes--poll-namespaces-process)
    (when (process-live-p proc)
      (kill-process proc)))
  (setq kubernetes--poll-namespaces-process nil))

(defvar kubernetes--poll-context-process nil
  "Single process used to prevent concurrent config refreshes.")

(defun kubernetes--set-poll-context-process (proc)
  (kubernetes--release-poll-context-process)
  (setq kubernetes--poll-context-process proc))

(defun kubernetes--release-poll-context-process ()
  (when-let (proc kubernetes--poll-context-process)
    (when (process-live-p proc)
      (kill-process proc)))
  (setq kubernetes--poll-context-process nil))

(defvar kubernetes--poll-pods-process nil
  "Single process used to prevent concurrent get pods requests.")

(defun kubernetes--set-poll-pods-process (proc)
  (kubernetes--release-poll-pods-process)
  (setq kubernetes--poll-pods-process proc))

(defun kubernetes--release-poll-pods-process ()
  (when-let (proc kubernetes--poll-pods-process)
    (when (process-live-p proc)
      (kill-process proc)))
  (setq kubernetes--poll-pods-process nil))

(defvar kubernetes--poll-configmaps-process nil
  "Single process used to prevent concurrent configmap refreshes.")

(defun kubernetes--set-poll-configmaps-process (proc)
  (kubernetes--release-poll-configmaps-process)
  (setq kubernetes--poll-configmaps-process proc))

(defun kubernetes--release-poll-configmaps-process ()
  (when-let (proc kubernetes--poll-configmaps-process)
    (when (process-live-p proc)
      (kill-process proc)))
  (setq kubernetes--poll-configmaps-process nil))

(defvar kubernetes--poll-services-process nil
  "Single process used to prevent concurrent service refreshes.")

(defun kubernetes--set-poll-services-process (proc)
  (kubernetes--release-poll-services-process)
  (setq kubernetes--poll-services-process proc))

(defun kubernetes--release-poll-services-process ()
  (when-let (proc kubernetes--poll-services-process)
    (when (process-live-p proc)
      (kill-process proc)))
  (setq kubernetes--poll-services-process nil))

(defvar kubernetes--poll-secrets-process nil
  "Single process used to prevent concurrent secret refreshes.")

(defun kubernetes--set-poll-secrets-process (proc)
  (kubernetes--release-poll-secrets-process)
  (setq kubernetes--poll-secrets-process proc))

(defun kubernetes--release-poll-secrets-process ()
  (when-let (proc kubernetes--poll-secrets-process)
    (when (process-live-p proc)
      (kill-process proc)))
  (setq kubernetes--poll-secrets-process nil))

(defun kubernetes--kill-process-quietly (proc)
  (when (and proc (process-live-p proc))
    (set-process-query-on-exit-flag proc nil)
    (set-process-sentinel proc nil)
    (set-process-buffer proc nil)
    (kill-process proc)))

(defun kubernetes--kill-polling-processes ()
  (mapc #'kubernetes--kill-process-quietly (list kubernetes--poll-pods-process
                                       kubernetes--poll-context-process
                                       kubernetes--poll-configmaps-process
                                       kubernetes--poll-secrets-process
                                       kubernetes--poll-services-process
                                       kubernetes--poll-namespaces-process))
  (setq kubernetes--poll-namespaces-process nil)
  (setq kubernetes--poll-configmaps-process nil)
  (setq kubernetes--poll-services-process nil)
  (setq kubernetes--poll-secrets-process nil)
  (setq kubernetes--poll-pods-process nil)
  (setq kubernetes--poll-context-process nil))


;; Timers
;;
;; A timer is used to poll Kubernetes to keep the pods list buffer up-to-date.

(defvar kubernetes--poll-timer nil
  "Background timer used to poll for updates.

This is used to regularly synchronise local state with Kubernetes.")

(defvar kubernetes--redraw-timer nil
  "Background timer used to trigger buffer redrawing.

This is used to display the current state.")

(defun kubernetes--initialize-timers ()
  (unless kubernetes--redraw-timer
    (setq kubernetes--redraw-timer (run-with-timer kubernetes-redraw-frequency kubernetes-redraw-frequency #'kubernetes--redraw-buffers)))
  (unless kubernetes--poll-timer
    (setq kubernetes--poll-timer (run-with-timer kubernetes-poll-frequency kubernetes-poll-frequency #'kubernetes-refresh))))

(defun kubernetes--kill-timers ()
  (when-let (timer kubernetes--redraw-timer)
    (cancel-timer timer))
  (when-let (timer kubernetes--poll-timer)
    (cancel-timer timer))
  (setq kubernetes--redraw-timer nil)
  (setq kubernetes--poll-timer nil))


;; Render AST Interpreter
;;
;; Implements an interpreter for a simple layout DSL for magit sections.

(defun kubernetes--eval-ast (render-ast)
  "Evaluate RENDER-AST in the context of the current buffer.

Warning: This could blow the stack if the AST gets too deep."
  (pcase render-ast
    (`(line . ,str)
     (insert str)
     (newline))

    (`(heading . ,str)
     (unless magit-insert-section--current
       (error "Inserting a heading, but not in a section"))
     (magit-insert-heading str))

    (`(section (,sym ,hide) ,inner-ast)
     (eval `(magit-insert-section (,sym nil ,hide)
              (kubernetes--eval-ast ',inner-ast))))

    (`(padding)
     (newline))

    (`(propertize ,spec ,inner-ast)
     (let ((start (point)))
       (kubernetes--eval-ast inner-ast)
       (add-text-properties start (point) spec)))

    ((and actions (pred listp))
     (dolist (action actions)
       (kubernetes--eval-ast action)))

    (x
     (error "Unknown AST form: %s" x))))


;; Context section rendering.

(defun kubernetes--render-context-section (state)
  `(section (context-container nil)
            (section (context nil)
                     (,(-let* (((&alist 'config config 'current-namespace current-namespace) state)
                               (current-context (-when-let ((&alist 'current-context current 'contexts contexts) config)
                                                  (--find (equal current (alist-get 'name it)) (append contexts nil)))))
                         (cond

                          ;; If a context is selected, draw that.
                          ((and config current-context)
                           (-let [(&alist 'name name 'context (&alist 'cluster cluster-name 'namespace ns)) current-context]
                             `((heading . ,(propertize (concat (format "%-12s" "Context: ") (propertize name 'face 'kubernetes-context-name)) 'kubernetes-copy name 'kubernetes-nav :display-config))
                               (line . ,(propertize (format "%-12s%s" "Cluster: " cluster-name) 'kubernetes-copy cluster-name 'kubernetes-nav :display-config))
                               (line . ,(propertize (format "%-12s%s" "Namespace: " (or current-namespace ns)) 'kubernetes-copy (or current-namespace ns) 'kubernetes-nav :display-config)))))

                          ;; If there is no context, draw the namespace.
                          (current-namespace
                           `((heading . ,(concat (format "%-12s" "Context: ") (propertize "<none>" 'face 'magit-dimmed)))
                             (line . ,(propertize (format "%-12s%s" "Namespace: " current-namespace) 'kubernetes-copy current-namespace))))

                          ;; If state is empty, assume requests are in progress.
                          (t
                           `((line . ,(concat (format "%-12s" "Context: ") (propertize "Fetching..." 'face 'kubernetes-progress-indicator)))))))

                      (padding)))))


;; Pod section rendering.

(defvar-local kubernetes--marked-pod-names nil)

(defvar-local kubernetes--pods-pending-deletion nil)

(defun kubernetes--format-pod-details (pod)
  (-let ((insert-detail
          (lambda (key value)
            (let ((str (concat (propertize (format "    %-12s" key) 'face 'magit-header-line)
                               value)))
              (cons 'line (concat (propertize str 'kubernetes-copy value))))))

         ((&alist 'metadata (&alist 'namespace ns 'labels (&alist 'name label-name))
                  'status (&alist 'containerStatuses [(&alist 'image image 'name name)]
                                  'hostIP host-ip
                                  'podIP pod-ip
                                  'startTime start-time))
          pod))
    (list (funcall insert-detail "Name:" name)
          (funcall insert-detail "Labels:" label-name)
          (funcall insert-detail "Namespace:" ns)
          (funcall insert-detail "Image:" image)
          (funcall insert-detail "Host IP:" host-ip)
          (funcall insert-detail "Pod IP:" pod-ip)
          (funcall insert-detail "Started:" start-time))))

(defun kubernetes--format-pod-line (pod current-time)
  (-let* (((&alist 'metadata (&alist 'name name)
                   'status (&alist 'containerStatuses containers
                                   'startTime start-time
                                   'phase phase))
           pod)
          ([(&alist 'restartCount restarts 'state state)] containers)
          (state (or (alist-get 'reason (alist-get 'waiting state))
                     phase))
          (str
           (concat
            ;; Name
            (format "%-45s " (kubernetes--ellipsize name 45))

            ;; State
            (let ((s (format "%-10s " (kubernetes--ellipsize state 10))))
              (if (equal state "Running") (propertize s 'face 'magit-dimmed) s))

            ;; Count
            (format "%5s "
                    (let* ((n-ready (seq-count (-lambda ((it &as &alist 'ready r))
                                                 (eq r t))
                                               containers))
                           (count-str (format "%s/%s" n-ready (seq-length containers))))
                      (if (zerop n-ready)
                          count-str
                        (propertize count-str 'face 'magit-dimmed))))

            ;; Restarts
            (let ((s (format "%8s " restarts)))
              (cond
               ((equal 0 restarts)
                (propertize s 'face 'magit-dimmed))
               ((<= kubernetes-pod-restart-warning-threshold restarts)
                (propertize s 'face 'warning))
               (t
                s)))

            ;; Age
            (let ((start (apply #'encode-time (kubernetes--parse-utc-timestamp start-time))))
              (propertize (format "%8s" (kubernetes--time-diff-string start current-time))
                          'face 'magit-dimmed))))

          (str (cond
                ((member (downcase state) '("running" "containercreating" "terminated"))
                 str)
                ((member (downcase state) '("runcontainererror" "crashloopbackoff"))
                 (propertize str 'face 'error))
                (t
                 (propertize str 'face 'warning))))
          (str
           (if (member name kubernetes--pods-pending-deletion)
               (concat (propertize str 'face 'kubernetes-pending-deletion))
             str))
          (str
           (if (member name kubernetes--marked-pod-names)
               (concat (propertize "D" 'face 'kubernetes-delete-mark) " " str)
             (concat "  " str))))
    (propertize str
                'kubernetes-nav (list :pod-name name)
                'kubernetes-copy name)))

(defun kubernetes--update-pod-marks-state (pods)
  (let ((pod-names (-map #'kubernetes--resource-name pods)))
    (setq kubernetes--pods-pending-deletion
          (-intersection kubernetes--pods-pending-deletion pod-names))
    (setq kubernetes--marked-pod-names
          (-intersection kubernetes--marked-pod-names pod-names))))

(defun kubernetes--render-pods-section (state &optional hidden)
  (-let* (((&alist 'current-time current-time
                   'pods (pods-response &as &alist 'items pods)) state)
          (pods (append pods nil))
          (column-heading (propertize (format "  %-45s %-10s %-5s   %6s %6s" "Name" "Status" "Ready" "Restarts" "Age")
                                      'face 'magit-section-heading)))
    `(section (pods-container ,hidden)
              (,(cond
                 ;; If the state is set and there are no pods, write "None".
                 ((and pods-response (null pods))
                  `((heading . ,(concat (propertize "Pods" 'face 'magit-header-line) " (0)"))
                    (section (pods-list nil)
                             (line . ,(propertize "  None." 'face 'magit-dimmed)))))

                 ;; If there are pods, write sections for each pods.
                 (pods
                  `((heading . ,(concat (propertize "Pods" 'face 'magit-header-line) " " (format "(%s)" (length pods))))
                    (line . ,column-heading)
                    ,@(--map `(section (,(intern (kubernetes--resource-name it)) t)
                                       ((heading . ,(kubernetes--format-pod-line it current-time))
                                        (section (details nil)
                                                 (,@(kubernetes--format-pod-details it)
                                                  (padding)))))
                             pods)))
                 ;; If there's no state, assume requests are in progress.
                 (t
                  `((heading . "Pods")
                    (section (pods-list nil)
                             ((line . ,column-heading)
                              (line . ,(propertize "  Fetching..." 'face 'kubernetes-progress-indicator)))))))
               (padding)))))


;; Configmap section rendering.

(defvar-local kubernetes--marked-configmap-names nil)

(defvar-local kubernetes--configmaps-pending-deletion nil)

(defun kubernetes--format-configmap-details (configmap)
  (-let ((insert-detail
          (lambda (key value)
            (let ((str (concat (propertize (format "    %-12s" key) 'face 'magit-header-line)
                               value)))
              (cons 'line (concat (propertize str 'kubernetes-copy value))))))

         ((&alist 'metadata (&alist 'namespace ns
                                    'creationTimestamp created-time))
          configmap))
    (list (funcall insert-detail "Namespace:" ns)
          (funcall insert-detail "Created:" created-time))))

(defun kubernetes--format-configmap-line (configmap current-time)
  (-let* (((&alist 'data data
                   'metadata (&alist 'name name 'creationTimestamp created-time))
           configmap)
          (str (concat
                ;; Name
                (format "%-45s " (kubernetes--ellipsize name 45))

                ;; Data
                (propertize (format "%6s " (seq-length data)) 'face 'magit-dimmed)

                ;; Age
                (let ((start (apply #'encode-time (kubernetes--parse-utc-timestamp created-time))))
                  (propertize (format "%6s" (kubernetes--time-diff-string start current-time))
                              'face 'magit-dimmed))))
          (str
           (if (member name kubernetes--configmaps-pending-deletion)
               (concat (propertize str 'face 'kubernetes-pending-deletion))
             str))
          (str
           (if (member name kubernetes--marked-configmap-names)
               (concat (propertize "D" 'face 'kubernetes-delete-mark) " " str)
             (concat "  " str))))
    (propertize str
                'kubernetes-nav (list :configmap-name name)
                'kubernetes-copy name)))

(defun kubernetes--render-configmaps-section (state &optional hidden)
  (-let* (((&alist 'current-time current-time
                   'configmaps (configmaps-response &as &alist 'items configmaps)) state)
          (configmaps (append configmaps nil))
          (column-heading (propertize (format "  %-45s %6s %6s" "Name" "Data" "Age") 'face 'magit-section-heading)))
    `(section (configmaps-container ,hidden)
              (,(cond
                 ;; If the state is set and there are no configmaps, write "None".
                 ((and configmaps-response (null configmaps))
                  `((heading . ,(concat (propertize "Configmaps" 'face 'magit-header-line) " (0)"))
                    (section (configmaps-list nil)
                             (line . ,(propertize "  None." 'face 'magit-dimmed)))))

                 ;; If there are configmaps, write sections for each configmaps.
                 (configmaps
                  `((heading . ,(concat (propertize "Configmaps" 'face 'magit-header-line) " " (format "(%s)" (length configmaps))))
                    (line . ,column-heading)
                    ,@(--map `(section (,(intern (kubernetes--resource-name it)) t)
                                       ((heading . ,(kubernetes--format-configmap-line it current-time))
                                        (section (details nil)
                                                 (,@(kubernetes--format-configmap-details it)
                                                  (padding)))))
                             configmaps)))
                 ;; If there's no state, assume requests are in progress.
                 (t
                  `((heading . "Configmaps")
                    (line . ,column-heading)
                    (section (configmaps-list nil)
                             (line . ,(propertize "  Fetching..." 'face 'kubernetes-progress-indicator))))))
               (padding)))))


;; Secret section rendering.

(defvar-local kubernetes--marked-secret-names nil)

(defvar-local kubernetes--secrets-pending-deletion nil)

(defun kubernetes--format-secret-details (secret)
  (-let ((insert-detail
          (lambda (key value)
            (let ((str (concat (propertize (format "    %-12s" key) 'face 'magit-header-line)
                               value)))
              (cons 'line (concat (propertize str 'kubernetes-copy value))))))

         ((&alist 'metadata (&alist 'namespace ns
                                    'creationTimestamp created-time))
          secret))
    (list (funcall insert-detail "Namespace:" ns)
          (funcall insert-detail "Created:" created-time))))

(defun kubernetes--format-secret-line (secret current-time)
  (-let* (((&alist 'data data
                   'metadata (&alist 'name name 'creationTimestamp created-time))
           secret)
          (str (concat
                ;; Name
                (format "%-45s " (kubernetes--ellipsize name 45))

                ;; Data
                (propertize (format "%6s " (seq-length data)) 'face 'magit-dimmed)

                ;; Age
                (let ((start (apply #'encode-time (kubernetes--parse-utc-timestamp created-time))))
                  (propertize (format "%6s" (kubernetes--time-diff-string start current-time))
                              'face 'magit-dimmed))))
          (str
           (if (member name kubernetes--secrets-pending-deletion)
               (concat (propertize str 'face 'kubernetes-pending-deletion))
             str))
          (str
           (if (member name kubernetes--marked-secret-names)
               (concat (propertize "D" 'face 'kubernetes-delete-mark) " " str)
             (concat "  " str))))
    (propertize str
                'kubernetes-nav (list :secret-name name)
                'kubernetes-copy name)))

(defun kubernetes--render-secrets-section (state &optional hidden)
  (-let* (((&alist 'current-time current-time
                   'secrets (secrets-response &as &alist 'items secrets)) state)
          (secrets (append secrets nil))
          (column-heading (propertize (format "  %-45s %6s %6s" "Name" "Data" "Age") 'face 'magit-section-heading)))
    `(section (secrets-container ,hidden)
              (,(cond
                 ;; If the state is set and there are no secrets, write "None".
                 ((and secrets-response (null secrets))
                  `((heading . ,(concat (propertize "Secrets" 'face 'magit-header-line) " (0)"))
                    (section (secrets-list nil)
                             (line . ,(propertize "  None." 'face 'magit-dimmed)))))

                 ;; If there are secrets, write sections for each secrets.
                 (secrets
                  `((heading . ,(concat (propertize "Secrets" 'face 'magit-header-line) " " (format "(%s)" (length secrets))))
                    (line . ,column-heading)
                    ,@(--map `(section (,(intern (kubernetes--resource-name it)) t)
                                       ((heading . ,(kubernetes--format-secret-line it current-time))
                                        (section (details nil)
                                                 (,@(kubernetes--format-secret-details it)
                                                  (padding)))))
                             secrets)))
                 ;; If there's no state, assume requests are in progress.
                 (t
                  `((heading . "Secrets")
                    (line . ,column-heading)
                    (section (secrets-list nil)
                             (line . ,(propertize "  Fetching..." 'face 'kubernetes-progress-indicator))))))
               (padding)))))


;; Service section rendering.

(defvar-local kubernetes--marked-service-names nil)

(defvar-local kubernetes--services-pending-deletion nil)

(defun kubernetes--format-service-details (service)
  (-let ((insert-detail
          (lambda (key value)
            (when value
              (let ((str (concat (propertize (format "    %-15s" key) 'face 'magit-header-line)
                                 value)))
                (cons 'line (concat (propertize str 'kubernetes-copy value)))))))

         (format-ports
          (-lambda ((&alist 'name name 'port port 'protocol prot))
            (concat (when name (format "%s:" name))
                    (number-to-string port) "/" prot)))

         ((&alist 'metadata (&alist 'namespace ns
                                    'creationTimestamp created-time)
                  'spec (&alist 'clusterIP internal-ip
                                'externalIPs ips
                                'ports ports))
          service))
    (-non-nil (list (funcall insert-detail "Namespace:" ns)
                    (funcall insert-detail "Created:" created-time)
                    (funcall insert-detail "Internal IP:" internal-ip)
                    (when-let (ips (append ips nil))
                      (funcall insert-detail "External IPs:" (string-join ips ", ")))
                    (when-let (ports (append ports nil))
                      (funcall insert-detail "Ports:" (string-join (-map format-ports ports) ", ")))))))

(defun kubernetes--format-service-line (service current-time)
  (-let* (((&alist 'metadata (&alist 'name name 'creationTimestamp created-time)
                   'spec (&alist 'clusterIP internal-ip
                                 'externalIPs external-ips))
           service)
          (str (concat
                ;; Name
                (format "%-30s " (kubernetes--ellipsize name 30))

                ;; Internal IP
                (propertize (format "%15s " internal-ip) 'face 'magit-dimmed)

                ;; External IP
                (let ((ips (append external-ips nil)))
                  (propertize (format "%15s " (or (car ips) "")) 'face 'magit-dimmed))

                ;; Age
                (let ((start (apply #'encode-time (kubernetes--parse-utc-timestamp created-time))))
                  (propertize (format "%6s" (kubernetes--time-diff-string start current-time))
                              'face 'magit-dimmed))))
          (str
           (if (member name kubernetes--services-pending-deletion)
               (concat (propertize str 'face 'kubernetes-pending-deletion))
             str))
          (str
           (if (member name kubernetes--marked-service-names)
               (concat (propertize "D" 'face 'kubernetes-delete-mark) " " str)
             (concat "  " str))))
    (propertize str
                'kubernetes-nav (list :service-name name)
                'kubernetes-copy name)))

(defun kubernetes--render-services-section (state &optional hidden)
  (-let* (((&alist 'current-time current-time
                   'services (services-response &as &alist 'items services)) state)
          (services (append services nil))
          (column-heading (propertize (format "  %-30s %15s %15s %6s" "Name" "Internal IP" "External IP" "Age") 'face 'magit-section-heading)))
    `(section (services-container ,hidden)
              (,(cond
                 ;; If the state is set and there are no services, write "None".
                 ((and services-response (null services))
                  `((heading . ,(concat (propertize "Services" 'face 'magit-header-line) " (0)"))
                    (section (services-list nil)
                             (line . ,(propertize "  None." 'face 'magit-dimmed)))))

                 ;; If there are services, write sections for each services.
                 (services
                  `((heading . ,(concat (propertize "Services" 'face 'magit-header-line) " " (format "(%s)" (length services))))
                    (line . ,column-heading)
                    ,@(--map `(section (,(intern (kubernetes--resource-name it)) t)
                                       ((heading . ,(kubernetes--format-service-line it current-time))
                                        (section (details nil)
                                                 (,@(kubernetes--format-service-details it)
                                                  (padding)))))
                             services)))
                 ;; If there's no state, assume requests are in progress.
                 (t
                  `((heading . "Services")
                    (line . ,column-heading)
                    (section (services-list nil)
                             (line . ,(propertize "  Fetching..." 'face 'kubernetes-progress-indicator))))))
               (padding)))))


;; Display pod view rendering routines.

(defun kubernetes--display-pods-initialize-buffer ()
  "Called the first time the pods buffer is opened to set up the buffer."
  (let ((buf (get-buffer-create kubernetes-display-pods-buffer-name)))
    (with-current-buffer buf
      (kubernetes-display-pods-mode)

      ;; Render buffer.
      (kubernetes--redraw-pods-buffer t)
      (goto-char (point-min))

      (kubernetes--initialize-timers)
      (add-hook 'kill-buffer-hook (kubernetes--make-cleanup-fn buf) nil t))
    buf))

(defun kubernetes--redraw-pods-buffer (&optional force)
  "Redraws the main buffer using the current state.

FORCE ensures it happens."
  (when-let (buf (get-buffer kubernetes-display-pods-buffer-name))
    (with-current-buffer buf
      (when (or force
                ;; HACK: Only redraw the buffer if it is in the selected window.
                ;;
                ;; The cursor moves unpredictably in a redraw, which ruins the current
                ;; position in the buffer if a popup window is open.
                (equal (window-buffer) buf))

        (let ((pos (point))
              (inhibit-read-only t)
              (inhibit-redisplay t)
              (state (kubernetes--state)))

          (-when-let ((&alist 'pods (&alist 'items pods)) state)
            (kubernetes--update-pod-marks-state (append pods nil)))

          (erase-buffer)
          (kubernetes--eval-ast `(section (root nil)
                                (,(kubernetes--render-context-section state)
                                 ,(kubernetes--render-pods-section state))))
          (goto-char pos)))

      ;; Force the section at point to highlight.
      (magit-section-update-highlight))))


;; Display configmap view rendering routines.

(defun kubernetes--display-configmaps-initialize-buffer ()
  "Called the first time the configmaps buffer is opened to set up the buffer."
  (let ((buf (get-buffer-create kubernetes-display-configmaps-buffer-name)))
    (with-current-buffer buf
      (kubernetes-display-configmaps-mode)

      ;; Render buffer.
      (kubernetes--redraw-configmaps-buffer t)
      (goto-char (point-min))

      (kubernetes--initialize-timers)
      (add-hook 'kill-buffer-hook (kubernetes--make-cleanup-fn buf) nil t))
    buf))

(defun kubernetes--redraw-configmaps-buffer (&optional force)
  "Redraws the main buffer using the current state.

FORCE ensures it happens."
  (when-let (buf (get-buffer kubernetes-display-configmaps-buffer-name))
    (with-current-buffer buf
      (when (or force
                ;; HACK: Only redraw the buffer if it is in the selected window.
                ;;
                ;; The cursor moves unpredictably in a redraw, which ruins the current
                ;; position in the buffer if a popup window is open.
                (equal (window-buffer) buf))

        (let ((pos (point))
              (inhibit-read-only t)
              (inhibit-redisplay t)
              (state (kubernetes--state)))

          (erase-buffer)
          (kubernetes--eval-ast `(section (root nil)
                                (,(kubernetes--render-context-section state)
                                 ,(kubernetes--render-configmaps-section state))))
          (goto-char pos)))

      ;; Force the section at point to highlight.
      (magit-section-update-highlight))))


;; Display secret view rendering routines.

(defun kubernetes--display-secrets-initialize-buffer ()
  "Called the first time the secrets buffer is opened to set up the buffer."
  (let ((buf (get-buffer-create kubernetes-display-secrets-buffer-name)))
    (with-current-buffer buf
      (kubernetes-display-secrets-mode)

      ;; Render buffer.
      (kubernetes--redraw-secrets-buffer t)
      (goto-char (point-min))

      (kubernetes--initialize-timers)
      (add-hook 'kill-buffer-hook (kubernetes--make-cleanup-fn buf) nil t))
    buf))

(defun kubernetes--redraw-secrets-buffer (&optional force)
  "Redraws the main buffer using the current state.

FORCE ensures it happens."
  (when-let (buf (get-buffer kubernetes-display-secrets-buffer-name))
    (with-current-buffer buf
      (when (or force
                ;; HACK: Only redraw the buffer if it is in the selected window.
                ;;
                ;; The cursor moves unpredictably in a redraw, which ruins the current
                ;; position in the buffer if a popup window is open.
                (equal (window-buffer) buf))

        (let ((pos (point))
              (inhibit-read-only t)
              (inhibit-redisplay t)
              (state (kubernetes--state)))

          (erase-buffer)
          (kubernetes--eval-ast `(section (root nil)
                                (,(kubernetes--render-context-section state)
                                 ,(kubernetes--render-secrets-section state))))
          (goto-char pos)))

      ;; Force the section at point to highlight.
      (magit-section-update-highlight))))


;; Displaying config.

(defun kubernetes-display-config-refresh (config)
  (let ((buf (get-buffer-create kubernetes-display-config-buffer-name)))
    (with-current-buffer buf
      (kubernetes-display-thing-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (kubernetes--json-to-yaml config))))
    buf))

;;;###autoload
(defun kubernetes-display-config (config)
  "Display information for CONFIG in a new window."
  (interactive (list (kubernetes--await-on-async #'kubernetes--kubectl-config-view)))
  (with-current-buffer (kubernetes-display-config-refresh config)
    (goto-char (point-min))
    (select-window (display-buffer (current-buffer)))))


;; Displaying configmaps.

(defun kubernetes-display-configmap-refresh (configmap-name)
  (if-let (configmap (kubernetes--state-lookup-configmap configmap-name))
      (let ((buf (get-buffer-create kubernetes-display-configmap-buffer-name)))
        (with-current-buffer buf
          (kubernetes-display-thing-mode)
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (kubernetes--json-to-yaml configmap))))
        buf)
    (error "Unknown configmap: %s" configmap-name)))

;;;###autoload
(defun kubernetes-display-configmap (configmap-name)
  "Display information for a configmap in a new window.

CONFIGMAP-NAME is the name of the configmap to display."
  (interactive (list (kubernetes--read-configmap-name)))
  (with-current-buffer (kubernetes-display-configmap-refresh configmap-name)
    (goto-char (point-min))
    (select-window (display-buffer (current-buffer)))))


;; Displaying secrets

(defun kubernetes-display-secret-refresh (secret-name)
  (if-let (secret (kubernetes--state-lookup-secret secret-name))
      (let ((buf (get-buffer-create kubernetes-display-secret-buffer-name)))
        (with-current-buffer buf
          (kubernetes-display-thing-mode)
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (kubernetes--json-to-yaml secret))))
        buf)
    (error "Unknown secret: %s" secret-name)))

;;;###autoload
(defun kubernetes-display-secret (secret-name)
  "Display information for a secret in a new window.

SECRET-NAME is the name of the secret to display."
  (interactive (list (kubernetes--read-secret-name)))
  (with-current-buffer (kubernetes-display-secret-refresh secret-name)
    (goto-char (point-min))
    (select-window (display-buffer (current-buffer)))))


;; Displaying services

(defun kubernetes-display-service-refresh (service-name)
  (if-let (service (kubernetes--state-lookup-service service-name))
      (let ((buf (get-buffer-create kubernetes-display-service-buffer-name)))
        (with-current-buffer buf
          (kubernetes-display-thing-mode)
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (kubernetes--json-to-yaml service))))
        buf)
    (error "Unknown service: %s" service-name)))

;;;###autoload
(defun kubernetes-display-service (service-name)
  "Display information for a service in a new window.

SERVICE-NAME is the name of the service to display."
  (interactive (list (kubernetes--read-service-name)))
  (with-current-buffer (kubernetes-display-service-refresh service-name)
    (goto-char (point-min))
    (select-window (display-buffer (current-buffer)))))


;; Displaying pods.

(defun kubernetes-display-pod-refresh (pod-name)
  (if-let (pod (kubernetes--state-lookup-pod pod-name))
      (let ((buf (get-buffer-create kubernetes-pod-buffer-name)))
        (with-current-buffer buf
          (kubernetes-display-thing-mode)
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (kubernetes--json-to-yaml pod))))
        buf)
    (error "Unknown pod: %s" pod-name)))

;;;###autoload
(defun kubernetes-display-pod (pod-name)
  "Display information for a pod in a new window.

POD-NAME is the name of the pod to display."
  (interactive (list (kubernetes--read-pod-name)))
  (with-current-buffer (kubernetes-display-pod-refresh pod-name)
    (goto-char (point-min))
    (select-window (display-buffer (current-buffer)))))


;; Marking pods for deletion

(defun kubernetes-mark-for-delete (point)
  "Mark the thing at POINT for deletion, then advance to the next line."
  (interactive "d")
  (pcase (get-text-property point 'kubernetes-nav)
    (`(:pod-name ,pod-name)
     (unless (member pod-name kubernetes--pods-pending-deletion)
       (add-to-list 'kubernetes--marked-pod-names pod-name)))
    (`(:configmap-name ,configmap-name)
     (unless (member configmap-name kubernetes--configmaps-pending-deletion)
       (add-to-list 'kubernetes--marked-configmap-names configmap-name)))
    (`(:secret-name ,secret-name)
     (unless (member secret-name kubernetes--secrets-pending-deletion)
       (add-to-list 'kubernetes--marked-secret-names secret-name)))
    (_
     (user-error "Nothing here can be marked")))
  (kubernetes--redraw-buffers)
  (goto-char point)
  (forward-line 1))

(defun kubernetes-unmark (point)
  "Unmark the thing at POINT, then advance to the next line."
  (interactive "d")
  (pcase (get-text-property point 'kubernetes-nav)
    (`(:pod-name ,pod-name)
     (setq kubernetes--marked-pod-names (delete pod-name kubernetes--marked-pod-names)))
    (`(:secret-name ,secret-name)
     (setq kubernetes--marked-secret-names (delete secret-name kubernetes--marked-secret-names)))
    (`(:configmap-name ,configmap-name)
     (setq kubernetes--marked-configmap-names (delete configmap-name kubernetes--marked-configmap-names))))
  (kubernetes--redraw-buffers)
  (goto-char point)
  (forward-line 1))

(defun kubernetes-unmark-all ()
  "Unmark everything in the buffer."
  (interactive)
  (setq kubernetes--marked-pod-names nil)
  (setq kubernetes--marked-configmap-names nil)
  (setq kubernetes--marked-secret-names nil)
  (let ((pt (point)))
    (kubernetes--redraw-buffers)
    (goto-char pt)))

(defun kubernetes-execute-marks ()
  "Action all marked items in the buffer."
  (interactive)
  (unless (or kubernetes--marked-pod-names
              kubernetes--marked-configmap-names
              kubernetes--marked-secret-names)
    (user-error "Nothing is marked"))

  (let ((n (length kubernetes--marked-pod-names)))
    (when (and (not (zerop n))
               (y-or-n-p (format "Delete %s pod%s? " n (if (equal 1 n) "" "s"))))
      (kubernetes--delete-marked-pods)
      (kubernetes-unmark-all)))

  (let ((n (length kubernetes--marked-configmap-names)))
    (when (and (not (zerop n))
               (y-or-n-p (format "Delete %s configmap%s? " n (if (equal 1 n) "" "s"))))
      (kubernetes--delete-marked-configmaps)
      (kubernetes-unmark-all)))

  (let ((n (length kubernetes--marked-secret-names)))
    (when (and (not (zerop n))
               (y-or-n-p (format "Delete %s secret%s? " n (if (equal 1 n) "" "s"))))
      (kubernetes--delete-marked-secrets)
      (kubernetes-unmark-all))))

(defun kubernetes--delete-marked-pods ()
  (let ((n (length kubernetes--marked-pod-names)))
    (message "Deleting %s pod%s..." n (if (equal 1 n) "" "s"))
    (dolist (pod kubernetes--marked-pod-names)
      (add-to-list 'kubernetes--pods-pending-deletion pod)

      (kubernetes--kubectl-delete-pod pod
                            (lambda (_)
                              (message "Deleting pod %s succeeded." pod)
                              (kubernetes-refresh))
                            (lambda (_)
                              (message "Deleting pod %s failed" pod)
                              (setq kubernetes--pods-pending-deletion (delete pod kubernetes--pods-pending-deletion)))))))

(defun kubernetes--delete-marked-configmaps ()
  (let ((n (length kubernetes--marked-configmap-names)))
    (message "Deleting %s configmap%s..." n (if (equal 1 n) "" "s"))
    (dolist (configmap kubernetes--marked-configmap-names)
      (add-to-list 'kubernetes--configmaps-pending-deletion configmap)

      (kubernetes--kubectl-delete-configmap configmap
                                  (lambda (_)
                                    (message "Deleting configmap %s succeeded." configmap)
                                    (kubernetes-refresh))
                                  (lambda (_)
                                    (message "Deleting configmap %s failed" configmap)
                                    (setq kubernetes--configmaps-pending-deletion (delete configmap kubernetes--configmaps-pending-deletion)))))))

(defun kubernetes--delete-marked-secrets ()
  (let ((n (length kubernetes--marked-secret-names)))
    (message "Deleting %s secret%s..." n (if (equal 1 n) "" "s"))
    (dolist (secret kubernetes--marked-secret-names)
      (add-to-list 'kubernetes--secrets-pending-deletion secret)

      (kubernetes--kubectl-delete-secret secret
                               (lambda (_)
                                 (message "Deleting secret %s succeeded." secret)
                                 (kubernetes-refresh))
                               (lambda (_)
                                 (message "Deleting secret %s failed" secret)
                                 (setq kubernetes--secrets-pending-deletion (delete secret kubernetes--secrets-pending-deletion)))))))


;;; Misc commands

(defun kubernetes-navigate (point)
  "Perform a context-sensitive navigation action.

Inspecs the `kubernetes-nav' text property at POINT to determine
how to navigate.  If that property is not found, no action is
taken."
  (interactive "d")
  (pcase (get-text-property point 'kubernetes-nav)
    (:display-config
     (kubernetes-display-config (alist-get 'config (kubernetes--state))))
    (`(:configmap-name ,configmap-name)
     (kubernetes-display-configmap configmap-name))
    (`(:service-name ,service-name)
     (kubernetes-display-service service-name))
    (`(:secret-name ,secret-name)
     (kubernetes-display-secret secret-name))
    (`(:pod-name ,pod-name)
     (kubernetes-display-pod pod-name))))

(defun kubernetes-copy-thing-at-point (point)
  "Perform a context-sensitive copy action.

Inspecs the `kubernetes-copy' text property at POINT to determine
what to copy."
  (interactive "d")
  (when-let (s (get-text-property point 'kubernetes-copy))
    (kill-new s)
    (message "Copied: %s" s)))

(defun kubernetes--redraw-buffers (&optional force)
  (kubernetes--redraw-pods-buffer force)
  (kubernetes--redraw-configmaps-buffer force)
  (kubernetes--redraw-secrets-buffer force)
  (kubernetes--redraw-overview-buffer force))

(defun kubernetes-refresh (&optional interactive)
  "Trigger a manual refresh Kubernetes pods buffers.

Requests the data needed to build the buffers.

With optional argument INTERACTIVE, redraw the buffer and log
additional information of state changes."
  (interactive (list t))
  ;; Make sure not to trigger a refresh if the buffer closes.
  (when (or (get-buffer kubernetes-display-configmaps-buffer-name)
            (get-buffer kubernetes-display-secrets-buffer-name)
            (get-buffer kubernetes-display-pods-buffer-name)
            (get-buffer kubernetes-overview-buffer-name))
    (when interactive
      (kubernetes--redraw-buffers)
      (message "Refreshing..."))

    (unless kubernetes--poll-namespaces-process
      (kubernetes--set-poll-namespaces-process
       (kubernetes--kubectl-get-namespaces
        (lambda (config)
          (setq kubernetes--get-namespaces-response config)
          (when interactive
            (message "Updated namespaces.")))
        (lambda ()
          (kubernetes--release-poll-namespaces-process)))))

    (unless kubernetes--poll-context-process
      (kubernetes--set-poll-context-process
       (kubernetes--kubectl-config-view
        (lambda (config)
          (setq kubernetes--view-config-response config)
          (when interactive
            (message "Updated contexts.")))
        (lambda ()
          (kubernetes--release-poll-context-process)))))

    (unless kubernetes--poll-configmaps-process
      (kubernetes--set-poll-configmaps-process
       (kubernetes--kubectl-get-configmaps
        (lambda (response)
          (setq kubernetes--get-configmaps-response response)
          (when interactive
            (message "Updated configmaps.")))
        (lambda ()
          (kubernetes--release-poll-configmaps-process)))))

    (unless kubernetes--poll-services-process
      (kubernetes--set-poll-services-process
       (kubernetes--kubectl-get-services
        (lambda (response)
          (setq kubernetes--get-services-response response)
          (when interactive
            (message "Updated services.")))
        (lambda ()
          (kubernetes--release-poll-services-process)))))

    (unless kubernetes--poll-secrets-process
      (kubernetes--set-poll-secrets-process
       (kubernetes--kubectl-get-secrets
        (lambda (response)
          (setq kubernetes--get-secrets-response response)
          (when interactive
            (message "Updated secrets.")))
        (lambda ()
          (kubernetes--release-poll-secrets-process)))))

    (unless kubernetes--poll-pods-process
      (kubernetes--set-poll-pods-process
       (kubernetes--kubectl-get-pods
        (lambda (response)
          (setq kubernetes--get-pods-response response)
          (when interactive
            (message "Updated pods.")))
        (lambda ()
          (kubernetes--release-poll-pods-process)))))))


;; Logs

(defun kubernetes--log-line-buffer-for-string (s)
  (let ((propertized (with-temp-buffer
                       (insert s)
                       (goto-char (point-min))
                       (when (equal (char-after) ?\{)
                         (json-pretty-print-buffer)
                         (funcall kubernetes-json-mode)
                         (font-lock-ensure))
                       (buffer-string))))

    (with-current-buffer (get-buffer-create kubernetes-log-line-buffer-name)
      (kubernetes-log-line-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert propertized)
        (goto-char (point-min)))
      (current-buffer))))

(defun kubernetes-logs-inspect-line (pos)
  "Show detail for the log line at POS."
  (interactive "d")
  (display-buffer (kubernetes--log-line-buffer-for-string
                   (save-excursion
                     (goto-char pos)
                     (buffer-substring (line-beginning-position) (line-end-position))))))

(defun kubernetes-logs-previous-line ()
  "Move backward and inspect the line at point."
  (interactive)
  (with-current-buffer kubernetes-logs-buffer-name
    (forward-line -1)
    (when (get-buffer kubernetes-log-line-buffer-name)
      (kubernetes-logs-inspect-line (point)))))

(defun kubernetes-logs-forward-line ()
  "Move forward and inspect the line at point."
  (interactive)
  (with-current-buffer kubernetes-logs-buffer-name
    (forward-line 1)
    (when (get-buffer kubernetes-log-line-buffer-name)
      (kubernetes-logs-inspect-line (point)))))


;; Popups

(magit-define-popup kubernetes-logs-popup
  "Popup console for logging commands for POD."
  :group 'kubernetes

  :options
  '("Options for customizing logging behaviour"
    (?t "Number of lines to display" "--tail=" read-number "-1")
    "Time controls"
    (?s "Since relative time" "--since=" kubernetes--read-time-value)
    (?d "Since absolute datetime" "--since-time=" kubernetes--read-iso-datetime))

  :switches
  '((?p "Print logs for previous instances of the container in this pod" "-p"))

  :actions
  '((?l "Logs" kubernetes-logs-fetch-all)
    (?f "Logs (stream and follow)" kubernetes-logs-follow))

  :max-action-columns 2

  :default-action 'kubernetes-logs)

(defun kubernetes-logs-follow (pod-name args)
  "Open a streaming logs buffer for a pod.

POD-NAME is the name of the pod to log.

ARGS are additional args to pass to kubectl."
  (interactive (list (or (kubernetes--maybe-pod-name-at-point) (kubernetes--read-pod-name))
                     (kubernetes-logs-arguments)))
  (kubernetes-logs-fetch-all pod-name (cons "-f" args)))

(defun kubernetes-logs-fetch-all (pod-name args)
  "Open a streaming logs buffer for POD.

POD-NAME is the name of the pod to log.

ARGS are additional args to pass to kubectl."
  (interactive (list (or (kubernetes--maybe-pod-name-at-point) (kubernetes--read-pod-name))
                     (kubernetes-logs-arguments)))
  (let ((compilation-buffer-name-function (lambda (_) kubernetes-logs-buffer-name))
        (command (append (list kubernetes-kubectl-executable "logs")
                         args
                         (list pod-name)
                         (when kubernetes--current-namespace
                           (list (format "--namespace=%s" kubernetes--current-namespace))))))
    (with-current-buffer (compilation-start (string-join command " ") 'kubernetes-logs-mode)
      (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)
      (select-window (display-buffer (current-buffer))))))


(magit-define-popup kubernetes-describe-popup
  "Popup console for describe commands."
  :group 'kubernetes

  :actions
  '((?d "Dwim" kubernetes-describe-dwim)
    (?p "Pod" kubernetes-describe-pod))

  :default-action 'kubernetes-logs)

(defun kubernetes--describable-thing-at-pt ()
  (save-excursion
    (back-to-indentation)
    (get-text-property (point) 'kubernetes-nav)))

(defun kubernetes-describe-dwim (thing)
  "Describe the thing at point.

THING must be a valid target for `kubectl describe'."
  (interactive (list (kubernetes--describable-thing-at-pt)))
  (pcase thing
    (`(:pod-name ,pod-name)
     (kubernetes-describe-pod pod-name))
    (_
     (user-error "Nothing at point to describe"))))

(defun kubernetes-describe-pod (pod-name)
  "Display a buffer for describing a pod.

POD-NAME is the name of the pod to describe."
  (interactive (list (or (kubernetes--maybe-pod-name-at-point) (kubernetes--read-pod-name))))
  (let ((buf (get-buffer-create kubernetes-pod-buffer-name))
        (marker (make-marker)))
    (with-current-buffer buf
      (kubernetes-display-thing-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (set-marker marker (point))
        (insert (propertize "Loading..." 'face 'magit-dimmed))))
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
           (proc (kubernetes--kubectl-describe-pod pod-name populate-buffer)))
      (with-current-buffer buf
        (add-hook 'kill-buffer-hook (lambda () (kubernetes--kill-process-quietly proc)) nil t)))

    (select-window (display-buffer buf))
    buf))


(magit-define-popup kubernetes-exec-popup
  "Popup console for exec commands for POD."
  :group 'kubernetes

  :default-arguments '("-i" "-t")

  :switches
  '((?i "Pass stdin to container" "-i" t)
    (?t "Stdin is a TTY" "-t" t))

  :actions
  '((?e "Exec" kubernetes-exec-into))

  :default-action 'kubernetes-exec-into)

(defun kubernetes-exec-into (pod-name args exec-command)
  "Open a terminal for execting into a pod.

POD-NAME is the name of the pod to exec into.

ARGS are additional args to pass to kubectl.

EXEC-COMMAND is the command to run in the container.

Should be invoked via command `kubernetes-logs-popup'."
  (interactive (list (or (kubernetes--maybe-pod-name-at-point) (kubernetes--read-pod-name))
                     (kubernetes-exec-arguments)
                     (read-string (format "Command (default: %s): " kubernetes-default-exec-command) nil 'kubernetes-exec-history)))

  (let* ((exec-command (cond
                        ((null exec-command)
                         kubernetes-default-exec-command)
                        ((string-empty-p (string-trim exec-command))
                         kubernetes-default-exec-command)
                        (t
                         (string-trim exec-command))))

         (command (append (list kubernetes-kubectl-executable "exec")
                          args
                          (when kubernetes--current-namespace
                            (list (format "--namespace=%s" kubernetes--current-namespace)))
                          (list pod-name exec-command)))

         (interactive-tty (member "-t" args))
         (mode (if interactive-tty
                   t ; Means use compilation-shell-minor-mode
                 #'kubernetes-logs-mode)))

    (with-current-buffer (compilation-start (string-join command " ") mode (lambda (_) kubernetes-pod-buffer-name))
      (when (and interactive-tty kubernetes-clean-up-interactive-exec-buffers)
        (make-local-variable 'compilation-finish-functions)
        (add-to-list 'compilation-finish-functions #'kubernetes--kill-compilation-buffer t))
      (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)
      (select-window (display-buffer (current-buffer))))))


(magit-define-popup kubernetes-config-popup
  "Popup console for showing an overview of available config commands."
  :group 'kubernetes
  :actions
  '("Managing contexts"
    (?c "Change context" kubernetes-use-context)
    "Query settings"
    (?n "Set namespace" kubernetes-set-namespace)))

(defun kubernetes-set-namespace (ns)
  "Set the namespace to query to NS, overriding the settings for the current context."
  (interactive (list (completing-read "Use namespace: " (kubernetes--namespace-names) nil t)))
  ;; The context is safe to preserve, but everything else should be reset.
  (let ((context kubernetes--view-config-response))
    (kubernetes--kill-polling-processes)
    (kubernetes--state-clear)
    (goto-char (point-min))
    (setq kubernetes--view-config-response context)
    (setq kubernetes--current-namespace ns)
    (kubernetes--redraw-buffers t)))

(defun kubernetes--namespace-names ()
  (-let* ((config (or kubernetes--get-namespaces-response (kubernetes--await-on-async #'kubernetes--kubectl-get-namespaces)))
          ((&alist 'items items) config))
    (-map (-lambda ((&alist 'metadata (&alist 'name name))) name) items)))

(defun kubernetes-use-context (context)
  "Switch Kubernetes context refresh the pods buffer.

CONTEXT is the name of a context as a string."
  (interactive (list (completing-read "Context: " (kubernetes--context-names) nil t)))
  (kubernetes--kill-polling-processes)
  (kubernetes--state-clear)
  (kubernetes--redraw-buffers t)
  (goto-char (point-min))
  (kubernetes--kubectl-config-use-context context (lambda (_)
                                          (kubernetes-refresh))))

(defun kubernetes--context-names ()
  (-let* ((config (or kubernetes--view-config-response (kubernetes--await-on-async #'kubernetes--kubectl-config-view)))
          ((&alist 'contexts contexts) config))
    (--map (alist-get 'name it) contexts)))


(magit-define-popup kubernetes-overview-popup
  "Popup console for showing an overview of available popup commands."
  :group 'kubernetes
  :actions
  '("Environment"
    (?c "Configuration" kubernetes-config-popup)
    "Marking pods"
    (?D "Delete pod at point" kubernetes-mark-for-delete)
    (?u "Unmark pod at point" kubernetes-unmark)
    (?U "Unmark all pods" kubernetes-unmark-all)
    "Popup commands"
    (?d "Describe" kubernetes-describe-popup)
    (?e "Exec" kubernetes-exec-popup)
    (?l "Logs" kubernetes-logs-popup)
    "Misc"
    (?h "Describe mode and keybindings" describe-mode)))


;; Mode definitions

;;;###autoload
(defvar kubernetes-mode-map
  (let ((keymap (make-sparse-keymap)))
    ;; Section controls
    (define-key keymap (kbd "p")   #'magit-section-backward)
    (define-key keymap (kbd "n")   #'magit-section-forward)
    (define-key keymap (kbd "M-p") #'magit-section-backward-sibling)
    (define-key keymap (kbd "M-n") #'magit-section-forward-sibling)
    (define-key keymap (kbd "C-i") #'magit-section-toggle)
    (define-key keymap (kbd "^")   #'magit-section-up)
    (define-key keymap [tab]       #'magit-section-toggle)
    (define-key keymap [C-tab]     #'magit-section-cycle)
    (define-key keymap [M-tab]     #'magit-section-cycle-diffs)
    (define-key keymap [S-tab]     #'magit-section-cycle-global)
    ;; Misc
    (define-key keymap (kbd "q") #'quit-window)
    (define-key keymap (kbd "RET") #'kubernetes-navigate)
    (define-key keymap (kbd "M-w") #'kubernetes-copy-thing-at-point)

    keymap)
  "Keymap for `kubernetes-mode'.  This is the base keymap for all derived modes.")

;;;###autoload
(define-derived-mode kubernetes-mode special-mode "Kubernetes"
  "Base mode for Kubernetes modes.

\\{kubernetes-mode-map}"
  :group 'kubernetes
  (read-only-mode)
  (buffer-disable-undo)
  (setq truncate-lines t)
  (setq-local line-move-visual t)
  (setq show-trailing-whitespace nil)
  (setq list-buffers-directory (abbreviate-file-name default-directory))
  (hack-dir-local-variables-non-file-buffer)
  (make-local-variable 'text-property-default-nonsticky)
  (push (cons 'keymap t) text-property-default-nonsticky)
  (push (cons 'kubernetes-nav t) text-property-default-nonsticky)
  (push (cons 'kubernetes-copy t) text-property-default-nonsticky)
  (add-hook 'post-command-hook #'magit-section-update-highlight t t)
  (setq-local redisplay-highlight-region-function 'magit-highlight-region)
  (setq-local redisplay-unhighlight-region-function 'magit-unhighlight-region)
  (when (bound-and-true-p global-linum-mode)
    (linum-mode -1))
  (when (and (fboundp 'nlinum-mode)
             (bound-and-true-p global-nlinum-mode))
    (nlinum-mode -1)))

;;;###autoload
(defvar kubernetes-display-pods-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "?") #'kubernetes-overview-popup)
    (define-key keymap (kbd "c") #'kubernetes-config-popup)
    (define-key keymap (kbd "d") #'kubernetes-describe-popup)
    (define-key keymap (kbd "D") #'kubernetes-mark-for-delete)
    (define-key keymap (kbd "e") #'kubernetes-exec-popup)
    (define-key keymap (kbd "g") #'kubernetes-refresh)
    (define-key keymap (kbd "u") #'kubernetes-unmark)
    (define-key keymap (kbd "U") #'kubernetes-unmark-all)
    (define-key keymap (kbd "x") #'kubernetes-execute-marks)
    (define-key keymap (kbd "l") #'kubernetes-logs-popup)
    (define-key keymap (kbd "h") #'describe-mode)
    keymap)
  "Keymap for `kubernetes-display-pods-mode'.")

;;;###autoload
(define-derived-mode kubernetes-display-pods-mode kubernetes-mode "Kubernetes Pods"
  "Mode for working with Kubernetes pods.

\\<kubernetes-display-pods-mode-map>\
Type \\[kubernetes-mark-for-delete] to mark a pod for deletion, and \\[kubernetes-execute-marks] to execute.
Type \\[kubernetes-unmark] to unmark the pod at point, or \\[kubernetes-unmark-all] to unmark all pods.

Type \\[kubernetes-navigate] to inspect the object on the current line.

Type \\[kubernetes-exec] to exec into a pod.

Type \\[kubernetes-logs] when point is on a pod to view its logs.

Type \\[kubernetes-copy-thing-at-point] to copy the pod name at point.

Type \\[kubernetes-refresh] to refresh the buffer.

\\{kubernetes-display-pods-mode-map}"
  :group 'kubernetes)

;;;###autoload
(defvar kubernetes-display-configmaps-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "?") #'kubernetes-overview-popup)
    (define-key keymap (kbd "c") #'kubernetes-config-popup)
    (define-key keymap (kbd "d") #'kubernetes-describe-popup)
    (define-key keymap (kbd "D") #'kubernetes-mark-for-delete)
    (define-key keymap (kbd "g") #'kubernetes-refresh)
    (define-key keymap (kbd "u") #'kubernetes-unmark)
    (define-key keymap (kbd "U") #'kubernetes-unmark-all)
    (define-key keymap (kbd "x") #'kubernetes-execute-marks)
    (define-key keymap (kbd "h") #'describe-mode)
    keymap)
  "Keymap for `kubernetes-display-configmaps-mode'.")

;;;###autoload
(define-derived-mode kubernetes-display-configmaps-mode kubernetes-mode "Kubernetes Configmaps"
  "Mode for working with Kubernetes configmaps.

\\<kubernetes-display-configmaps-mode-map>\
Type \\[kubernetes-mark-for-delete] to mark a configmap for deletion, and \\[kubernetes-execute-marks] to execute.
Type \\[kubernetes-unmark] to unmark the configmap at point, or \\[kubernetes-unmark-all] to unmark all configmaps.

Type \\[kubernetes-navigate] to inspect the object on the current line.

Type \\[kubernetes-copy-thing-at-point] to copy the configmap name at point.

Type \\[kubernetes-refresh] to refresh the buffer.

\\{kubernetes-display-configmaps-mode-map}"
  :group 'kubernetes)

;;;###autoload
(defvar kubernetes-display-secrets-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "?") #'kubernetes-overview-popup)
    (define-key keymap (kbd "c") #'kubernetes-config-popup)
    (define-key keymap (kbd "d") #'kubernetes-describe-popup)
    (define-key keymap (kbd "D") #'kubernetes-mark-for-delete)
    (define-key keymap (kbd "g") #'kubernetes-refresh)
    (define-key keymap (kbd "u") #'kubernetes-unmark)
    (define-key keymap (kbd "U") #'kubernetes-unmark-all)
    (define-key keymap (kbd "x") #'kubernetes-execute-marks)
    (define-key keymap (kbd "h") #'describe-mode)
    keymap)
  "Keymap for `kubernetes-display-secrets-mode'.")

;;;###autoload
(define-derived-mode kubernetes-display-secrets-mode kubernetes-mode "Kubernetes Secrets"
  "Mode for working with Kubernetes secrets.

\\<kubernetes-display-secrets-mode-map>\
Type \\[kubernetes-mark-for-delete] to mark a secret for deletion, and \\[kubernetes-execute-marks] to execute.
Type \\[kubernetes-unmark] to unmark the secret at point, or \\[kubernetes-unmark-all] to unmark all secrets.

Type \\[kubernetes-navigate] to inspect the object on the current line.

Type \\[kubernetes-copy-thing-at-point] to copy the secret name at point.

Type \\[kubernetes-refresh] to refresh the buffer.

\\{kubernetes-display-secrets-mode-map}"
  :group 'kubernetes)

;;;###autoload
(defvar kubernetes-logs-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "n") #'kubernetes-logs-forward-line)
    (define-key keymap (kbd "p") #'kubernetes-logs-previous-line)
    (define-key keymap (kbd "RET") #'kubernetes-logs-inspect-line)
    keymap)
  "Keymap for `kubernetes-logs-mode'.")

;;;###autoload
(define-compilation-mode kubernetes-logs-mode "Kubernetes Logs"
  "Mode for displaying and inspecting Kubernetes logs.

\\<kubernetes-logs-mode-map>\
Type \\[kubernetes-logs-inspect-line] to open the line at point in a new buffer.

\\{kubernetes-logs-mode-map}")

;;;###autoload
(defvar kubernetes-log-line-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "n") #'kubernetes-logs-forward-line)
    (define-key keymap (kbd "p") #'kubernetes-logs-previous-line)
    keymap)
  "Keymap for `kubernetes-log-line-mode'.")

;;;###autoload
(define-compilation-mode kubernetes-log-line-mode "Log Line"
  "Mode for inspecting Kubernetes log lines.

\\{kubernetes-log-line-mode-map}"
  (read-only-mode)
  (setq-local compilation-error-regexp-alist nil)
  (setq-local compilation-error-regexp-alist-alist nil))

;;;###autoload
(define-derived-mode kubernetes-display-thing-mode kubernetes-mode "Kubernetes Object"
  "Mode for inspecting a Kubernetes object.

\\{kubernetes-display-thing-mode-map}"
  :group 'kubernetes)

;;;###autoload
(defvar kubernetes-overview-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "?") #'kubernetes-overview-popup)
    (define-key keymap (kbd "c") #'kubernetes-config-popup)
    (define-key keymap (kbd "d") #'kubernetes-describe-popup)
    (define-key keymap (kbd "g") #'kubernetes-refresh)
    (define-key keymap (kbd "u") #'kubernetes-unmark)
    (define-key keymap (kbd "U") #'kubernetes-unmark-all)
    (define-key keymap (kbd "x") #'kubernetes-execute-marks)
    (define-key keymap (kbd "h") #'describe-mode)
    keymap)
  "Keymap for `kubernetes-overview-mode'.")

;;;###autoload
(define-derived-mode kubernetes-overview-mode kubernetes-mode "Kubernetes Overview"
  "Mode for working with Kubernetes overview.

\\<kubernetes-overview-mode-map>\
Type \\[kubernetes-mark-for-delete] to mark an object for deletion, and \\[kubernetes-execute-marks] to execute.
Type \\[kubernetes-unmark] to unmark the object at point, or \\[kubernetes-unmark-all] to unmark all objects.

Type \\[kubernetes-navigate] to inspect the object on the current line.

Type \\[kubernetes-copy-thing-at-point] to copy the thing at point.

Type \\[kubernetes-refresh] to refresh the buffer.

\\{kubernetes-overview-mode-map}"
  :group 'kubernetes)


;; Main entrypoints.

;;;###autoload
(defun kubernetes-display-pods ()
  "Display a list of pods in the current Kubernetes context."
  (interactive)
  (kubernetes-display-buffer (kubernetes--display-pods-initialize-buffer))
  (message (substitute-command-keys "\\<kubernetes-display-pods-mode-map>Type \\[kubernetes-overview-popup] for usage.")))

;;;###autoload
(defun kubernetes-display-configmaps ()
  "Display a list of configmaps in the current Kubernetes context."
  (interactive)
  (kubernetes-display-buffer (kubernetes--display-configmaps-initialize-buffer))
  (message (substitute-command-keys "\\<kubernetes-display-configmaps-mode-map>Type \\[kubernetes-overview-popup] for usage.")))

;;;###autoload
(defun kubernetes-display-secrets ()
  "Display a list of secrets in the current Kubernetes context."
  (interactive)
  (kubernetes-display-buffer (kubernetes--display-secrets-initialize-buffer))
  (message (substitute-command-keys "\\<kubernetes-display-secrets-mode-map>Type \\[kubernetes-overview-popup] for usage.")))


;; Overview

(defun kubernetes--overview-initialize-buffer ()
  "Called the first time the overview buffer is opened to set up the buffer."
  (let ((buf (get-buffer-create kubernetes-overview-buffer-name)))
    (with-current-buffer buf
      (kubernetes-overview-mode)

      ;; Render buffer.
      (kubernetes--redraw-overview-buffer t)
      (goto-char (point-min))

      (kubernetes--initialize-timers)
      (add-hook 'kill-buffer-hook (kubernetes--make-cleanup-fn buf) nil t))
    buf))

(defun kubernetes--redraw-overview-buffer (&optional force)
  "Redraws the main buffer using the current state.

FORCE ensures it happens."
  (when-let (buf (get-buffer kubernetes-overview-buffer-name))
    (with-current-buffer buf
      (when (or force
                ;; HACK: Only redraw the buffer if it is in the selected window.
                ;;
                ;; The cursor moves unpredictably in a redraw, which ruins the current
                ;; position in the buffer if a popup window is open.
                (equal (window-buffer) buf))

        (let ((pos (point))
              (inhibit-read-only t)
              (inhibit-redisplay t)
              (state (kubernetes--state)))

          (erase-buffer)
          (kubernetes--eval-ast `(section (root nil)
                                (,(kubernetes--render-context-section state)
                                 ,(kubernetes--render-configmaps-section state t)
                                 ,(kubernetes--render-pods-section state t)
                                 ,(kubernetes--render-secrets-section state t)
                                 ,(kubernetes--render-services-section state t))))
          (goto-char pos)))

      ;; Force the section at point to highlight.
      (magit-section-update-highlight))))


;;;###autoload
(defun kubernetes-overview ()
  "Display an overview buffer for Kubernetes."
  (interactive)
  (kubernetes-display-buffer (kubernetes--overview-initialize-buffer))
  (message (substitute-command-keys "\\<kubernetes-overview-mode-map>Type \\[kubernetes-overview-popup] for usage.")))

(provide 'kubernetes)

;;; kubernetes.el ends here
