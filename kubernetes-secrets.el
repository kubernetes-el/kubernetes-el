;;; kubernetes-secrets.el --- Rendering routines for Kubernetes secrets.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)

(require 'kubernetes-ast)
(require 'kubernetes-core)
(require 'kubernetes-loading-container)
(require 'kubernetes-modes)
(require 'kubernetes-state)
(require 'kubernetes-utils)
(require 'kubernetes-yaml)


;; Components

(defconst kubernetes-secrets--column-heading
  ["%-45s %6s %6s" "Name Data Age"])

(defconst kubernetes-secrets--default-columns
  '((Name (width -45))
    (Data (width 6))
    (Age (width 6)))
  "Possible columns to select for resource-type secrets")

(kubernetes-ast-define-component secret-detail (secret)
  (-let [(&alist 'metadata (&alist 'namespace ns 'creationTimestamp time)) secret]
    `((section (namespace nil)
               (nav-prop (:namespace-name ,ns)
                         (key-value 12 "Namespace" ,(propertize ns 'face 'kubernetes-namespace))))
      (key-value 12 "Created" ,time))))

(kubernetes-ast-define-component secret-line (state secret)
  ;; (when (not (alist-get 'secrets-columns state))
  ;;   (setf (alist-get 'secrets-columns state) kubernetes-secrets--default-columns))
  (-let* ((current-time (kubernetes-state--get state 'current-time))
          (pending-deletion (kubernetes-state--get state 'secrets-pending-deletion))
          (marked-secrets (kubernetes-state--get state 'marked-secrets))
          ((&alist 'data data 'metadata (&alist 'name name 'creationTimestamp created-time))
           secret)
          (line
           (-let* ((row "")
                   ((&alist 'secrets-columns secrets-columns) state))
             ;; Read the formatting for the table from the kubernetes-pods--default-columns variable
             (dotimes (i (length secrets-columns))
               ;; Read the column-width (and create format-string) and header for the current column
               (let* ((col (nth i secrets-columns))
                      (col-name (car col))
                      (props (cdr col))
                      (width (car (alist-get 'width props)))
                      (fmt (concat "%" (number-to-string width) "s")))
                 ;; Depending on the value of the header we use a specific print function.
                 (setq row (concat row (pcase  col-name
                                         ('Name
                                          (format fmt (s-truncate (abs width) name))
                                          )
                                         ('Data
                                          (propertize (format fmt (seq-length data)) 'face 'kubernetes-dimmed))
                                         ('Age
                                          (let ((start (apply #'encode-time (kubernetes-utils-parse-utc-timestamp created-time))))
                                            (propertize (format fmt (kubernetes--time-diff-string start current-time))
                                                        'face 'kubernetes-dimmed)))
                                         (_
                                          (format "%s " (format fmt "?"))
                                          ))
                                   (unless (= i (1- (length secrets-columns))) " ")))))
             row)))
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
  (-let* (((&alist 'secrets-columns column-settings) state)
         ((&alist 'items secrets) (kubernetes-state--get state 'secrets))
         ([fmt labels] (kubernetes-utils--create-table-headers column-settings)))
    `(section (secrets-container ,hidden)
              (header-with-count "Secrets" ,secrets)
              (indent
               (columnar-loading-container ,secrets
                                           ,(propertize
                                             (apply #'format fmt (split-string labels "|"))
                                             'face
                                             'magit-section-heading)
                                           ,(--map `(secret ,state ,it) secrets)))
              (padding))))

;; Requests and state management

(kubernetes-state-define-refreshers secrets)

(defun kubernetes-secrets-delete-marked (state)
  (let ((names (kubernetes-state--get state 'marked-secrets)))
    (dolist (name names)
      (kubernetes-state-delete-secret name)
      (kubernetes-kubectl-delete "secret" name state
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
           (or (kubernetes-state--get state 'secrets)
               (progn
                 (message "Getting secrets...")
                 (let ((response (kubernetes-kubectl-await-on-async state (-partial #'kubernetes-kubectl-get "secrets"))))
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
