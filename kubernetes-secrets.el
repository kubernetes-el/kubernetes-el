;;; kubernetes-secrets.el --- Rendering routines for Kubernetes secrets.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)

(require 'kubernetes-ast)
(require 'kubernetes-loading-container)
(require 'kubernetes-modes)
(require 'kubernetes-props)
(require 'kubernetes-state)
(require 'kubernetes-utils)
(require 'kubernetes-yaml)


;; Components

(defconst kubernetes-secrets--column-heading
  (propertize (format "%-45s %6s %6s" "Name" "Data" "Age")
              'face 'magit-section-heading))

(kubernetes-ast-define-component secret-detail (secret)
  (-let [(&alist 'metadata (&alist 'namespace ns 'creationTimestamp time)) secret]
    `((section (namespace nil)
               (nav-prop (:namespace-name ,ns)
                         (key-value 12 "Namespace" ,(propertize ns 'face 'kubernetes-namespace))))
      (key-value 12 "Created" ,time))))

(kubernetes-ast-define-component secret-line (state secret)
  (-let* ((current-time (kubernetes-state-current-time state))
          (pending-deletion (kubernetes-state-secrets-pending-deletion state))
          (marked-secrets (kubernetes-state-marked-secrets state))
          ((&alist 'data data 'metadata (&alist 'name name 'creationTimestamp created-time))
           secret)
          (line `(line ,(concat
                         ;; Name
                         (format "%-45s " (kubernetes-utils-ellipsize name 45))

                         ;; Data
                         (propertize (format "%6s " (seq-length data)) 'face 'magit-dimmed)

                         ;; Age
                         (let ((start (apply #'encode-time (kubernetes-utils-parse-utc-timestamp created-time))))
                           (propertize (format "%6s" (kubernetes-utils-time-diff-string start current-time))
                                       'face 'magit-dimmed))))))

    `(nav-prop (:secret-name ,name)
               (copy-prop ,name
                          ,(cond
                            ((member name pending-deletion)
                             `(propertize (face kubernetes-pending-deletion) ,line))
                            ((member name marked-secrets)
                             `(mark-for-delete ,line))
                            (t
                             line))))))

(kubernetes-ast-define-component secret (state secret)
  `(section (,(intern (kubernetes-state-resource-name secret)) t)
            (heading (secret-line ,state ,secret))
            (section (details nil)
                     (indent
                      (secret-detail ,secret)
                      (padding)))))

(kubernetes-ast-define-component secrets-list (state &optional hidden)
  (-let [(&alist 'items secrets) (kubernetes-state-secrets state)]
    `(section (secrets-container ,hidden)
              (header-with-count "Secrets" ,secrets)
              (indent
               (columnar-loading-container ,secrets ,kubernetes-secrets--column-heading
                                           ,(--map `(secret ,state ,it) secrets)))
              (padding))))


;; Requests and state management

(defun kubernetes-secrets-refresh (&optional interactive)
  (unless (kubernetes-process-poll-secrets-process-live-p)
    (kubernetes-process-set-poll-secrets-process
     (kubernetes-kubectl-get-secrets kubernetes-props
                                     (kubernetes-state)
                                     (lambda (response)
                                       (kubernetes-state-update-secrets response)
                                       (when interactive
                                         (message "Updated secrets.")))
                                     (lambda ()
                                       (kubernetes-process-release-poll-secrets-process))))))

(defun kubernetes-secrets-delete-marked (state)
  (let ((names (kubernetes-state-marked-secrets state)))
    (dolist (name names)
      (kubernetes-state-delete-secret name)
      (kubernetes-kubectl-delete-secret kubernetes-props state name
                                        (lambda (_)
                                          (message "Deleting secret %s succeeded." name))
                                        (lambda (_)
                                          (message "Deleting secret %s failed" name)
                                          (kubernetes-state-mark-secret name))))
    (kubernetes-state-trigger-redraw)))


;; Displaying secrets.

(defun kubernetes-secrets--read-name (state)
  "Read a secret name from the user.

STATE is the current application state.

Update the secret state if it not set yet."
  (-let* (((&alist 'items secrets)
           (or (kubernetes-state-secrets state)
               (progn
                 (message "Getting secrets...")
                 (let ((response (kubernetes-kubectl-await-on-async kubernetes-props state #'kubernetes-kubectl-get-secrets)))
                   (kubernetes-state-update-secrets response)
                   response))))
          (secrets (append secrets nil))
          (names (-map #'kubernetes-state-resource-name secrets)))
    (completing-read "Secret: " names nil t)))

;;;###autoload
(defun kubernetes-display-secret (secret-name state)
  "Display information for a secret in a new window.

STATE is the current application state.

SECRET-NAME is the name of the secret to display."
  (interactive (let ((state (kubernetes-state)))
                 (list (kubernetes-secrets--read-name state) state)))
  (if-let (secret (kubernetes-state-lookup-secret secret-name state))
      (select-window
       (display-buffer
        (kubernetes-yaml-make-buffer kubernetes-display-secret-buffer-name secret)))
    (error "Unknown secret: %s" secret-name)))


(provide 'kubernetes-secrets)

;;; kubernetes-secrets.el ends here
