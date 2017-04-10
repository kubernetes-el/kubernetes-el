;;; kubernetes-configmaps.el --- Rendering for Kubernetes configmaps.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)

(require 'kubernetes-state)
(require 'kubernetes-utils)


(defun kubernetes-configmaps--format-detail (configmap)
  (-let [(&alist 'metadata (&alist 'namespace ns 'creationTimestamp time)) configmap]
    `((copy-prop ,ns (key-value 12 "Namespace" ,ns))
      (copy-prop ,time (key-value 12 "Created" ,time)))))

(defun kubernetes-configmaps--format-line (configmap current-time)
  (-let* (((&alist 'data data
                   'metadata (&alist 'name name 'creationTimestamp created-time))
           configmap)
          (line `(line ,(concat
                         ;; Name
                         (format "%-45s " (kubernetes--ellipsize name 45))

                         ;; Data
                         (propertize (format "%6s " (seq-length data)) 'face 'magit-dimmed)

                         ;; Age
                         (let ((start (apply #'encode-time (kubernetes--parse-utc-timestamp created-time))))
                           (propertize (format "%6s" (kubernetes--time-diff-string start current-time))
                                       'face 'magit-dimmed))))))
    `(nav-prop (:configmap-name ,name)
               (copy-prop ,name
                          ,(cond
                            ((member name kubernetes-state--configmaps-pending-deletion)
                             `(propertize (face kubernetes-pending-deletion) ,line))
                            ((member name kubernetes-state--marked-configmap-names)
                             `(mark-for-delete ,line))
                            (t
                             line))))))

(defun kubernetes-configmaps-render (state &optional hidden)
  (-let* (((&alist 'current-time current-time
                   'configmaps (configmaps-response &as &alist 'items configmaps)) state)
          (configmaps (append configmaps nil))
          (column-heading (propertize (format "%-45s %6s %6s" "Name" "Data" "Age") 'face 'magit-section-heading)))
    `(section (configmaps-container ,hidden)
              ,(cond
                ;; If the state is set and there are no configmaps, write "None".
                ((and configmaps-response (null configmaps))
                 `((heading ,(concat (propertize "Configmaps" 'face 'magit-header-line) " (0)"))
                   (section (configmaps-list nil)
                            (indent
                             (propertize (face magit-dimmed) (line "None."))))))

                ;; If there are configmaps, write sections for each configmaps.
                (configmaps
                 (let ((make-entry
                        (lambda (it)
                          `(section (,(intern (kubernetes-state-resource-name it)) t)
                                    (heading ,(kubernetes-configmaps--format-line it current-time))
                                    (section (details nil)
                                             (indent
                                              ,@(kubernetes-configmaps--format-detail it)
                                              (padding)))))))

                   `((heading ,(concat (propertize "Configmaps" 'face 'magit-header-line) " " (format "(%s)" (length configmaps))))
                     (indent
                      (line ,column-heading)
                      ,@(-map make-entry configmaps)))))

                ;; If there's no state, assume requests are in progress.
                (t
                 `((heading "Configmaps")
                   (indent
                    (line ,column-heading)
                    (section (configmaps-list nil)
                             (propertize (face kubernetes-progress-indicator) (line "Fetching...")))))))
              (padding))))


(provide 'kubernetes-configmaps)

;;; kubernetes-configmaps.el ends here
