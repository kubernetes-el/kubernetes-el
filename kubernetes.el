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
  "The background refresh frequency in seconds."
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

(defvar kubernetes--view-context-response nil
  "State representing the view context response from the API.

Used to draw the context section of the main buffer.")

(defvar kubernetes--get-namespaces-response nil
  "State representing the namespaces response from the API.

Used for namespace selection within a cluster.")

(defvar kubernetes--current-namespace nil
  "The namespace to use in queries.  Overrides the context settings.")

(defvar kubernetes--pod-to-exec nil
  "Identifies the pod to exec into after querying the user for flags.

Assigned before opening the exec popup, when the target pod is
likely to be at point.  After choosing flags, this is the pod that
will be exec'ed into.

This variable is reset after use by the exec functions.")

(defvar kubernetes--pod-to-log nil
  "Identifies the pod to log after querying the user for flags.

Assigned before opening the logging popup, when the target pod is
likely to be at point.  After choosing flags, this is the pod that
will be logged.

This variable is reset after use by the logging functions.")

(defvar kubernetes--thing-to-describe nil
  "Identifies the thing to log for `kubernetes-describe-dwim'.

When set, it is the value of the 'kubernetes-nav' property at point.

Assigned before opening the describe popup, when the target is
likely to be at point.  If `kubernetes-describe-dwim' is selected
in the popup, this is the thing that will be inspected.

This variable is reset after use by the logging functions.")

(defun kubernetes--clear-main-state ()
  (setq kubernetes--get-pods-response nil)
  (setq kubernetes--view-context-response nil)
  (setq kubernetes--get-namespaces-response nil)
  (setq kubernetes--current-namespace nil)
  (setq kubernetes--pod-to-exec nil)
  (setq kubernetes--pod-to-log nil)
  (setq kubernetes--thing-to-describe nil))


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

(defun kubernetes--pod-name (pod)
  (-let [(&alist 'metadata (&alist 'name name)) pod]
    name))

(defun kubernetes--read-pod ()
  (-let* (((&alist 'items pods)
           (or kubernetes--get-pods-response
               (progn
                 (message "Getting pods...")
                 (kubernetes--await-on-async #'kubernetes--kubectl-get-pods))))
          (pods (append pods nil))
          (names (-map #'kubernetes--pod-name pods))
          (choice (completing-read "Pod: " names nil t)))
    (--find (equal choice (kubernetes--pod-name it)) pods)))

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

(defun kubernetes--maybe-pod-at-point ()
  (pcase (get-text-property (point) 'kubernetes-nav)
    (`(:pod ,pod)
     pod)))

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
                                       ((numberp v) (number-to-string v))
                                       ((and (stringp v) (< (length v) kubernetes-yaml-string-drop-threshold)) v)
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

(defun kubernetes--kill-process-quietly (proc)
  (when (and proc (process-live-p proc))
    (set-process-query-on-exit-flag proc nil)
    (set-process-sentinel proc nil)
    (set-process-buffer proc nil)
    (kill-process proc)))

(defun kubernetes--kill-polling-processes ()
  (mapc #'kubernetes--kill-process-quietly (list kubernetes--poll-pods-process
                                       kubernetes--poll-context-process
                                       kubernetes--poll-namespaces-process))
  (setq kubernetes--poll-namespaces-process nil)
  (setq kubernetes--poll-pods-process nil)
  (setq kubernetes--poll-context-process nil))


;; Timers
;;
;; A timer is used to poll Kubernetes to keep the pods list buffer up-to-date.

(defvar kubernetes--poll-timer nil
  "Background timer used to poll for updates.

This is used to regularly synchronise local state with Kubernetes.")

(defun kubernetes--initialize-timers ()
  (setq kubernetes--poll-timer (run-with-timer kubernetes-poll-frequency kubernetes-poll-frequency #'kubernetes-refresh)))

(defun kubernetes--kill-timers ()
  (when-let (timer kubernetes--poll-timer)
    (cancel-timer timer))
  (setq kubernetes--poll-timer nil))


;; View management

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

(defun kubernetes-display-pod-refresh (pod)
  (let ((buf (get-buffer-create kubernetes-pod-buffer-name)))
    (with-current-buffer buf
      (kubernetes-display-thing-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (kubernetes--json-to-yaml pod))))
    buf))

;;;###autoload
(defun kubernetes-display-pod (pod)
  "Display information for POD in a new window."
  (interactive (list (kubernetes--read-pod)))
  (with-current-buffer (kubernetes-display-pod-refresh pod)
    (goto-char (point-min))
    (select-window (display-buffer (current-buffer)))))


;; Context section rendering.

(defun kubernetes--context-section-lines (namespace-state config)
  (with-temp-buffer
    (-let* (((&alist 'current-context current 'contexts contexts) config)
            (lines
             (list
              (propertize (concat
                           (format "%-12s" "Context: ")
                           (propertize (or current "<none>") 'face 'kubernetes-context-name))
                          'kubernetes-copy current)

              (-when-let* ((ctx (--find (equal current (alist-get 'name it)) (append contexts nil)))
                           ((&alist 'name n 'context (&alist 'cluster c 'namespace ns)) ctx))
                (list (unless (string-empty-p c)
                        (propertize (format "%-12s%s" "Cluster: " c)
                                    'kubernetes-copy c))
                      (propertize
                       (if (and ns namespace-state)
                           (format "%-12s%s" "Namespace: " namespace-state)
                         (format "%-12s%s" "Namespace: " (or ns namespace-state)))
                       'kubernetes-copy (or namespace-state ns)))))))

      (--map (propertize it 'kubernetes-nav (list :config config))
             (-non-nil (-flatten lines))))))

(defun kubernetes--draw-context-section (namespace-state config)
  (magit-insert-section (context-container)
    (magit-insert-section (context)
      (cond
       (config
        (-let [(context . lines) (kubernetes--context-section-lines namespace-state config)]
          (magit-insert-heading (concat context "\n"))
          (insert (string-join lines "\n"))))
       (namespace-state
        (magit-insert-heading (concat (format "%-12s" "Context: ") (propertize "<none>" 'face 'magit-dimmed)))
        (insert (propertize (format "%-12s%s" "Namespace: " namespace-state) 'kubernetes-copy namespace-state))
        (newline))
       (t
        (insert (concat (format "%-12s" "Context: ") (propertize "Fetching..." 'face 'magit-dimmed)))))
      (newline 2))))


;; Pod section rendering.

(defvar-local kubernetes--marked-pod-names nil)

(defvar-local kubernetes--pods-pending-deletion nil)

(defun kubernetes--format-pod-details (pod)
  (with-temp-buffer
    (-let ((insert-detail
            (lambda (key value)
              (let ((str (concat (propertize (format "    %-12s" key) 'face 'magit-header-line)
                                 value)))
                (insert (concat (propertize str 'kubernetes-copy value)))
                (newline))))

           ((&alist 'metadata (&alist 'namespace ns 'labels (&alist 'name label-name))
                    'status (&alist 'containerStatuses [(&alist 'image image 'name name)]
                                    'hostIP host-ip
                                    'podIP pod-ip
                                    'startTime start-time))
            pod))
      (funcall insert-detail "Name:" name)
      (funcall insert-detail "Labels:" label-name)
      (funcall insert-detail "Namespace:" ns)
      (funcall insert-detail "Image:" image)
      (funcall insert-detail "Host IP:" host-ip)
      (funcall insert-detail "Pod IP:" pod-ip)
      (funcall insert-detail "Started:" start-time)
      (buffer-string))))

(defun kubernetes--format-pod-line (pod)
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
            (let* ((start (apply #'encode-time (kubernetes--parse-utc-timestamp start-time)))
                   (now (current-time)))
              (propertize (format "%8s" (kubernetes--time-diff-string start now))
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
                'kubernetes-nav (list :pod pod)
                'kubernetes-copy name)))

(defun kubernetes--update-pod-marks-state (pods)
  (let ((pod-names (-map #'kubernetes--pod-name pods)))
    (setq kubernetes--pods-pending-deletion
          (-intersection kubernetes--pods-pending-deletion pod-names))
    (setq kubernetes--marked-pod-names
          (-intersection kubernetes--marked-pod-names pod-names))))

(defun kubernetes--draw-pods-section (get-pods-response)
  (-let (((&alist 'items pods) get-pods-response)
         (column-heading (propertize (format "  %-45s %-10s %-5s   %6s %6s\n" "Name" "Status" "Ready" "Restarts" "Age")
                                     'face 'magit-section-heading)))
    (kubernetes--update-pod-marks-state pods)
    (magit-insert-section (pods-container)
      (cond
       ((and get-pods-response (null (append pods nil)))
        (magit-insert-heading "Pods")
        (magit-insert-section (pods-list)
          (insert (propertize "  None." 'face 'magit-dimmed))
          (newline)))
       (pods
        (magit-insert-heading (concat (propertize "Pods" 'face 'magit-header-line) " " (format "(%s)" (length pods))))
        (insert column-heading)
        (dolist (pod (append pods nil))
          (magit-insert-section ((eval (intern (kubernetes--pod-name pod))) nil t)
            (magit-insert-heading (kubernetes--format-pod-line pod))
            (magit-insert-section (details)
              (insert (kubernetes--format-pod-details pod))
              (insert ?\n)))))
       (t
        (magit-insert-heading "Pods")
        (magit-insert-section (pods-list)
          (insert column-heading)
          (insert (propertize "  Fetching..." 'face 'kubernetes-progress-indicator))
          (newline)))))))


;; Root rendering routines.

(defun kubernetes--display-pods-initialize-buffer ()
  "Called the first time the pods buffer is opened to set up the buffer."
  (let ((buf (get-buffer-create kubernetes-display-pods-buffer-name)))
    (with-current-buffer buf
      (kubernetes-display-pods-mode)

      ;; Render buffer.
      (kubernetes--redraw-main-buffer t)
      (goto-char (point-min))

      (kubernetes--initialize-timers)

      (add-hook 'kill-buffer-hook
                (lambda ()
                  (with-current-buffer buf
                    (kubernetes--clear-main-state)
                    (kubernetes--kill-polling-processes)
                    (kubernetes--kill-timers)))
                nil t))
    buf))

(defun kubernetes--redraw-main-buffer (&optional force)
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
              (inhibit-redisplay t))
          (erase-buffer)
          (magit-insert-section (root)
            (kubernetes--draw-context-section kubernetes--current-namespace kubernetes--view-context-response)
            (kubernetes--draw-pods-section kubernetes--get-pods-response))

          (goto-char pos)))

      ;; Force the section at point to highlight.
      (magit-section-update-highlight))))


;; Marking pods for deletion

(defun kubernetes-mark-for-delete (point)
  "Mark the thing at POINT for deletion, then advance to the next line."
  (interactive "d")
  (pcase (get-text-property point 'kubernetes-nav)
    (`(:pod ,pod)
     (let ((name (kubernetes--pod-name pod)))
       (unless (member name kubernetes--pods-pending-deletion)
         (add-to-list 'kubernetes--marked-pod-names name)
         (kubernetes--redraw-main-buffer))))
    (_
     (user-error "Nothing here can be marked")))
  (goto-char point)
  (forward-line 1))

(defun kubernetes-unmark (point)
  "Unmark the thing at POINT, then advance to the next line."
  (interactive "d")
  (pcase (get-text-property point 'kubernetes-nav)
    (`(:pod ,pod)
     (let ((name (kubernetes--pod-name pod)))
       (setq kubernetes--marked-pod-names (delete name kubernetes--marked-pod-names))
       (kubernetes--redraw-main-buffer))))
  (goto-char point)
  (forward-line 1))

(defun kubernetes-unmark-all ()
  "Unmark everything in the buffer."
  (interactive)
  (setq kubernetes--marked-pod-names nil)
  (let ((pt (point)))
    (kubernetes--redraw-main-buffer)
    (goto-char pt)))

(defun kubernetes-execute-marks ()
  "Action all marked items in the buffer."
  (interactive)
  (unless kubernetes--marked-pod-names
    (user-error "Nothing is marked"))

  (let ((n (length kubernetes--marked-pod-names)))
    (if (y-or-n-p (format "Execute %s mark%s? " n (if (equal 1 n) "" "s")))
        (progn
          (message "Deleting %s pod%s..." n (if (equal 1 n) "" "s"))
          (dolist (pod kubernetes--marked-pod-names)
            (add-to-list 'kubernetes--pods-pending-deletion pod)

            (kubernetes--kubectl-delete-pod pod
                                  (lambda (_)
                                    (message "Deleting pod %s succeeded." pod)
                                    (kubernetes-refresh))
                                  (lambda (_)
                                    (message "Deleting pod %s failed" pod)
                                    (setq kubernetes--pods-pending-deletion (delete pod kubernetes--pods-pending-deletion)))))

          (kubernetes-unmark-all))
      (message "Cancelled."))))


;;; Misc commands

(defun kubernetes-navigate (point)
  "Perform a context-sensitive navigation action.

Inspecs the `kubernetes-nav' text property at POINT to determine
how to navigate.  If that property is not found, no action is
taken."
  (interactive "d")
  (pcase (get-text-property point 'kubernetes-nav)
    (`(:config ,config)
     (kubernetes-display-config config))
    (`(:pod ,pod)
     (kubernetes-display-pod pod))))

(defun kubernetes-copy-thing-at-point (point)
  "Perform a context-sensitive copy action.

Inspecs the `kubernetes-copy' text property at POINT to determine
what to copy."
  (interactive "d")
  (when-let (s (get-text-property point 'kubernetes-copy))
    (kill-new s)
    (message "Copied: %s" s)))

(defun kubernetes-refresh (&optional verbose)
  "Trigger a manual refresh the Kubernetes pods buffer.

Requests the data needed to build the buffer, and updates the UI
state as responses arrive.

With optional argument VERBOSE, log additional information of
state changes."
  (interactive (list t))
  ;; Make sure not to trigger a refresh if the buffer closes.
  (when (get-buffer kubernetes-display-pods-buffer-name)
    (when verbose
      (message "Refreshing..."))

    (kubernetes--redraw-main-buffer)

    (unless kubernetes--poll-namespaces-process
      (kubernetes--set-poll-namespaces-process
       (kubernetes--kubectl-get-namespaces
        (lambda (config)
          (setq kubernetes--get-namespaces-response config)
          (when verbose
            (message "Updated namespaces.")))
        (lambda ()
          (kubernetes--release-poll-namespaces-process)))))

    (unless kubernetes--poll-context-process
      (kubernetes--set-poll-context-process
       (kubernetes--kubectl-config-view
        (lambda (config)
          (setq kubernetes--view-context-response config)
          (kubernetes--redraw-main-buffer)
          (when verbose
            (message "Updated contexts.")))
        (lambda ()
          (kubernetes--release-poll-context-process)))))

    (unless kubernetes--poll-pods-process
      (kubernetes--set-poll-pods-process
       (kubernetes--kubectl-get-pods
        (lambda (response)
          (setq kubernetes--get-pods-response response)
          (kubernetes--redraw-main-buffer)
          (when verbose
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

(defun kubernetes-logs (pod)
  "Popup console for logging commands for POD."
  (interactive (list (or (kubernetes--maybe-pod-at-point) (kubernetes--read-pod))))
  (setq kubernetes--pod-to-log pod)
  (call-interactively #'kubernetes-logs-popup))

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

(defun kubernetes-logs-follow ()
  "Open a streaming logs buffer for a pod.

Should be invoked via command `kubernetes-logs-popup'."
  (interactive)
  (kubernetes-logs-fetch-all (cons "-f" (kubernetes-logs-arguments))))

(defun kubernetes-logs-fetch-all (args)
  "Open a streaming logs buffer for a pod.

ARGS are additional args to pass to kubectl.

Should be invoked via command `kubernetes-logs-popup'."
  (interactive (list (kubernetes-logs-arguments)))
  (let* ((name (kubernetes--pod-name kubernetes--pod-to-log))
         (compilation-buffer-name-function (lambda (_) kubernetes-logs-buffer-name))
         (command (append (list kubernetes-kubectl-executable "logs")
                          args
                          (list name)
                          (when kubernetes--current-namespace
                            (list (format "--namespace=%s" kubernetes--current-namespace))))))
    (setq kubernetes--pod-to-log nil)
    (with-current-buffer (compilation-start (string-join command " ") 'kubernetes-logs-mode)
      (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)
      (select-window (display-buffer (current-buffer))))))

(defun kubernetes--describable-thing-at-pt ()
  (save-excursion
    (back-to-indentation)
    (get-text-property (point) 'kubernetes-nav)))

(defun kubernetes-describe (&optional thing)
  "Popup console for describing things.

THING is the thing to be used if the user selects
`kubernetes-describe-dwim'"
  (interactive (list (kubernetes--describable-thing-at-pt)))
  (setq kubernetes--thing-to-describe thing)
  (call-interactively #'kubernetes-describe-popup))

(magit-define-popup kubernetes-describe-popup
  "Popup console for describe commands."
  :group 'kubernetes

  :actions
  '((?d "Dwim" kubernetes-describe-dwim)
    (?p "Pod" kubernetes-describe-pod))

  :default-action 'kubernetes-logs)

(defun kubernetes-describe-dwim (thing)
  "Describe the thing at point.

THING must be a valid target for `kubectl describe'."
  (interactive (list (or kubernetes--thing-to-describe (kubernetes--describable-thing-at-pt))))
  (pcase thing
    (`(:pod ,pod)
     (kubernetes-describe-pod pod))
    (_
     (user-error "Nothing at point to describe")))
  (setq kubernetes--thing-to-describe nil))

(defun kubernetes-describe-pod (pod)
  "Display a buffer for describing POD."
  (interactive (list (or (kubernetes--maybe-pod-at-point) (kubernetes--read-pod))))
  (let ((buf (get-buffer-create kubernetes-pod-buffer-name))
        (pod-name (kubernetes--pod-name pod))
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

(defun kubernetes-exec (pod)
  "Popup console for exec'ing into POD."
  (interactive (list (or (kubernetes--maybe-pod-at-point) (kubernetes--read-pod))))
  (setq kubernetes--pod-to-exec pod)
  (call-interactively #'kubernetes-exec-popup))

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

(defun kubernetes-exec-into (args exec-command)
  "Open a terminal for execting into a pod.

ARGS are additional args to pass to kubectl.

EXEC-COMMAND is the command to run in the container.

Should be invoked via command `kubernetes-logs-popup'."
  (interactive (list (kubernetes-exec-arguments)
                     (read-string (format "Command (default: %s): " kubernetes-default-exec-command) nil 'kubernetes-exec-history)))
  (let* ((name (kubernetes--pod-name kubernetes--pod-to-exec))

         (exec-command (cond
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
                          (list name exec-command)))

         (interactive-tty (member "-t" args))
         (mode (if interactive-tty
                   t ; Means use compilation-shell-minor-mode
                 #'kubernetes-logs-mode)))

    (setq kubernetes--pod-to-exec nil)
    (with-current-buffer (compilation-start (string-join command " ") mode (lambda (_) kubernetes-pod-buffer-name))
      (when (and interactive-tty kubernetes-clean-up-interactive-exec-buffers)
        (make-local-variable 'compilation-finish-functions)
        (add-to-list 'compilation-finish-functions #'kubernetes--kill-compilation-buffer t))
      (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)
      (select-window (display-buffer (current-buffer))))))

(defun kubernetes--kill-compilation-buffer (proc-buf &rest _)
  (if-let (win (get-buffer-window proc-buf))
      (quit-window t win)
    (kill-buffer proc-buf)))

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
  (let ((context kubernetes--view-context-response))
    (kubernetes--kill-polling-processes)
    (kubernetes--clear-main-state)
    (goto-char (point-min))
    (setq kubernetes--view-context-response context)
    (setq kubernetes--current-namespace ns)
    (kubernetes--redraw-main-buffer t)))

(defun kubernetes--namespace-names ()
  (-let* ((config (or kubernetes--get-namespaces-response (kubernetes--await-on-async #'kubernetes--kubectl-get-namespaces)))
          ((&alist 'items items) config))
    (-map (-lambda ((&alist 'metadata (&alist 'name name))) name) items)))

(defun kubernetes-use-context (context)
  "Switch Kubernetes context refresh the pods buffer.

CONTEXT is the name of a context as a string."
  (interactive (list (completing-read "Context: " (kubernetes--context-names) nil t)))
  (kubernetes--kill-polling-processes)
  (kubernetes--clear-main-state)
  (kubernetes--redraw-main-buffer t)
  (goto-char (point-min))
  (kubernetes--kubectl-config-use-context context (lambda (_)
                                          (kubernetes-refresh))))

(defun kubernetes--context-names ()
  (-let* ((config (or kubernetes--view-context-response (kubernetes--await-on-async #'kubernetes--kubectl-config-view)))
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
    (define-key keymap (kbd "TAB") #'magit-section-toggle)
    (define-key keymap (kbd "c") #'kubernetes-config-popup)
    (define-key keymap (kbd "d") #'kubernetes-describe)
    (define-key keymap (kbd "D") #'kubernetes-mark-for-delete)
    (define-key keymap (kbd "e") #'kubernetes-exec)
    (define-key keymap (kbd "g") #'kubernetes-refresh)
    (define-key keymap (kbd "u") #'kubernetes-unmark)
    (define-key keymap (kbd "U") #'kubernetes-unmark-all)
    (define-key keymap (kbd "x") #'kubernetes-execute-marks)
    (define-key keymap (kbd "l") #'kubernetes-logs)
    (define-key keymap (kbd "h") #'describe-mode)
    keymap)
  "Keymap for `kubernetes-display-pods-mode'.")

;;;###autoload
(define-derived-mode kubernetes-display-pods-mode kubernetes-mode "Kubernetes Pods"
  "Mode for working with Kubernetes pods.

\\<kubernetes-display-pods-mode-map>\
Type \\[kubernetes-mark-for-delete] to mark a pod for deletion, and \\[kubernetes-execute-marks] to execute.
Type \\[kubernetes-unmark] to unmark the pod at point, or \\[kubernetes-unmark-all] to unmark all pods.

Type \\[kubernetes-navigate] to inspect the object on the current line, and \\[kubernetes-describe-pod] to
specifically describe a pod.

Type \\[kubernetes-exec] to exec into a pod.

Type \\[kubernetes-logs] when point is on a pod to view its logs.

Type \\[kubernetes-copy-thing-at-point] to copy the pod name at point.

Type \\[kubernetes-refresh] to refresh the buffer.

\\{kubernetes-display-pods-mode-map}"
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
(defun kubernetes-display-pods ()
  "Display a list of pods in the current Kubernetes context."
  (interactive)
  (kubernetes-display-buffer (kubernetes--display-pods-initialize-buffer))
  (message (substitute-command-keys "\\<kubernetes-display-pods-mode-map>Type \\[kubernetes-overview-popup] for usage.")))

(provide 'kubernetes)

;;; kubernetes.el ends here
