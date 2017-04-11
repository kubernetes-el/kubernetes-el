;;; kubernetes-secrets.el --- Rendering routines for Kubernetes secrets.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)

(require 'kubernetes-state)
(require 'kubernetes-utils)


(defun kubernetes-secrets--format-detail (secret)
  (-let [(&alist 'metadata (&alist 'namespace ns 'creationTimestamp time)) secret]
    `((copy-prop ,ns (key-value 12 "Namespace" ,ns))
      (copy-prop ,time (key-value 12 "Created" ,time)))))

(defun kubernetes-secrets--format-line (secret current-time)
  (-let* (((&alist 'data data 'metadata (&alist 'name name 'creationTimestamp created-time))
           secret)
          (line `(line ,(concat
                         ;; Name
                         (format "%-45s " (kubernetes--ellipsize name 45))

                         ;; Data
                         (propertize (format "%6s " (seq-length data)) 'face 'magit-dimmed)

                         ;; Age
                         (let ((start (apply #'encode-time (kubernetes--parse-utc-timestamp created-time))))
                           (propertize (format "%6s" (kubernetes--time-diff-string start current-time))
                                       'face 'magit-dimmed))))))

    `(nav-prop (:secret-name ,name)
               (copy-prop ,name
                          ,(cond
                            ((member name kubernetes-state--secrets-pending-deletion)
                             `(propertize (face kubernetes-pending-deletion) ,line))
                            ((member name kubernetes-state--marked-secret-names)
                             `(mark-for-delete ,line))
                            (t
                             line))))))

(defun kubernetes-secrets-render (state &optional hidden)
  (-let* (((&alist 'current-time current-time
                   'secrets (secrets-response &as &alist 'items secrets)) state)
          (secrets (append secrets nil))
          (column-heading (propertize (format "%-45s %6s %6s" "Name" "Data" "Age") 'face 'magit-section-heading)))
    `(section (secrets-container ,hidden)
              ,(cond
                ;; If the state is set and there are no secrets, write "None".
                ((and secrets-response (null secrets))
                 `((heading ,(concat (propertize "Secrets" 'face 'magit-header-line) " (0)"))
                   (indent
                    (section (secrets-list nil)
                             (propertize (face magit-dimmed) (line "None."))))))

                ;; If there are secrets, write sections for each secret.
                (secrets
                 (let ((make-entry
                        (lambda (it)
                          `(section (,(intern (kubernetes-state-resource-name it)) t)
                                    (heading ,(kubernetes-secrets--format-line it current-time))
                                    (section (details nil)
                                             (indent
                                              ,@(kubernetes-secrets--format-detail it)
                                              (padding)))))))
                   `((heading ,(concat (propertize "Secrets" 'face 'magit-header-line) " " (format "(%s)" (length secrets))))
                     (indent
                      (line ,column-heading)
                      ,@(-map make-entry secrets)))))

                ;; If there's no state, assume requests are in progress.
                (t
                 `((heading "Secrets")
                   (indent
                    (line ,column-heading)
                    (section (secrets-list nil)
                             (propertize (face kubernetes-progress-indicator) (line "Fetching...")))))))
              (padding))))


(provide 'kubernetes-secrets)

;;; kubernetes-secrets.el ends here
