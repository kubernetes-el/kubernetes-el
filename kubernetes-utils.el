;;; kubernetes-utils.el --- Common utilities.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 'subr-x)

(require 'kubernetes-kubectl)
(require 'kubernetes-state)
(require 'kubernetes-timers)

(autoload 'org-read-date "org")

(defun kubernetes-utils-read-pod-name (state)
  "Read a pod name from the user.

STATE is the current application state.

Update the pod state if it not set yet."
  (-let* (((&alist 'items pods)
           (or (kubernetes-state-pods state)
               (progn
                 (message "Getting pods...")
                 (let ((response (kubernetes-kubectl-await-on-async kubernetes-default-props state #'kubernetes-kubectl-get-pods)))
                   (kubernetes-state-update-pods response)
                   response))))
          (pods (append pods nil))
          (names (-map #'kubernetes-state-resource-name pods)))
    (completing-read "Pod: " names nil t)))

(defun kubernetes-utils-read-configmap-name (state)
  "Read a configmap name from the user.

STATE is the current application state.

Update the configmap state if it not set yet."
  (-let* (((&alist 'items configmaps)
           (or (kubernetes-state-configmaps state)
               (progn
                 (message "Getting configmaps...")
                 (let ((response (kubernetes-kubectl-await-on-async kubernetes-default-props state #'kubernetes-kubectl-get-configmaps)))
                   (kubernetes-state-update-configmaps response)
                   response))))
          (configmaps (append configmaps nil))
          (names (-map #'kubernetes-state-resource-name configmaps)))
    (completing-read "Configmap: " names nil t)))

(defun kubernetes-utils-read-secret-name (state)
  "Read a secret name from the user.

STATE is the current application state.

Update the secret state if it not set yet."
  (-let* (((&alist 'items secrets)
           (or (kubernetes-state-secrets state)
               (progn
                 (message "Getting secrets...")
                 (let ((response (kubernetes-kubectl-await-on-async kubernetes-default-props state #'kubernetes-kubectl-get-secrets)))
                   (kubernetes-state-update-secrets response)
                   response))))
          (secrets (append secrets nil))
          (names (-map #'kubernetes-state-resource-name secrets)))
    (completing-read "Secret: " names nil t)))

(defun kubernetes-utils-read-service-name (state)
  "Read a service name from the user.

STATE is the current application state.

Update the service state if it not set yet."
  (-let* (((&alist 'items services)
           (or (kubernetes-state-services state)
               (progn
                 (message "Getting services...")
                 (let ((response (kubernetes-kubectl-await-on-async kubernetes-default-props state #'kubernetes-kubectl-get-services)))
                   (kubernetes-state-update-services response)
                   response))))
          (services (append services nil))
          (names (-map #'kubernetes-state-resource-name services)))
    (completing-read "Service: " names nil t)))

(defun kubernetes-utils-read-iso-datetime (&rest _)
  (let* ((date (org-read-date nil t))
         (tz (format-time-string "%z" date)))
    (concat
     (format-time-string "%Y-%m-%dT%H:%M:%S" date)
     (replace-regexp-in-string (rx (group (? (any "+-")) digit digit)
                                   (group digit digit))
                               "\\1:\\2"
                               tz))))

(defun kubernetes-utils-read-time-value (&rest _)
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

(defun kubernetes-utils-maybe-pod-name-at-point ()
  (pcase (get-text-property (point) 'kubernetes-nav)
    (`(:pod-name ,value)
     value)))

(defun kubernetes-utils-json-to-yaml (json &optional level)
  "Process some parsed JSON and pretty-print as YAML.

JSON is a parsed JSON value.

LEVEL indentation level to use.  It defaults to 0 if not supplied."
  (let* ((level (or level 0))
         (space (string-to-char " "))
         (indentation (make-string (* level kubernetes-yaml-indentation-width) space))
         (body
          (cond
           ((vectorp json)
            (let* ((list-items (--map (string-trim-left (kubernetes-utils-json-to-yaml it (1+ level)))
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
                                      (propertize (format "%s: " k) 'face 'kubernetes-json-key)
                                      (cond
                                       ((equal t v) "true")
                                       ((equal :json-false v) "false")
                                       ((equal nil v) "null")

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
                                        (concat "\n" (kubernetes-utils-json-to-yaml v (1+ level)))))))
                            json)))
              (string-join entries "\n")))
           (t
            (format "%s%s" indentation json)))))
    (if (= 0 level)
        (concat (propertize "---\n" 'face 'magit-dimmed) body)
      body)))

(defun kubernetes-utils-ellipsize (s threshold)
  (if (> (length s) threshold)
      (concat (substring s 0 (1- threshold)) "…")
    s))

(defun kubernetes-utils-parse-utc-timestamp (timestamp)
  "Parse TIMESTAMP string from the API into the representation used by Emacs."
  (let ((parsed (parse-time-string (replace-regexp-in-string "Z" "" (replace-regexp-in-string "T" " " timestamp)))))
    (setf (nth 8 parsed) 0)
    parsed))

(defun kubernetes-utils-time-diff-string (start now)
  "Find the interval between START and NOW, and return a string of the coarsest unit."
  (let ((diff (time-to-seconds (time-subtract now start))))
    (car (split-string (format-seconds "%yy,%dd,%hh,%mm,%ss%z" diff) ","))))

(defun kubernetes-utils-kill-buffer (proc-buf &rest _)
  (if-let (win (get-buffer-window proc-buf))
      (quit-window t win)
    (kill-buffer proc-buf)))

(defun kubernetes-utils-make-cleanup-fn (buf)
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
            (kubernetes-state-clear)))
        (kubernetes-process-kill-polling-processes)
        (kubernetes-timers-kill-timers)))))

(provide 'kubernetes-utils)

;;; kubernetes-utils.el ends here
