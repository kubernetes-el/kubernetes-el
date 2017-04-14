;;; kubernetes-configmaps.el --- Rendering for Kubernetes configmaps.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)

(require 'kubernetes-modes)
(require 'kubernetes-process)
(require 'kubernetes-state)
(require 'kubernetes-utils)


;; Component

(defun kubernetes-configmaps--format-detail (configmap)
  (-let [(&alist 'metadata (&alist 'namespace ns 'creationTimestamp time)) configmap]
    `((copy-prop ,ns (key-value 12 "Namespace" ,ns))
      (copy-prop ,time (key-value 12 "Created" ,time)))))

(defun kubernetes-configmaps--format-line (state configmap)
  (-let* ((current-time (kubernetes-state-current-time state))
          (pending-deletion (kubernetes-state-configmaps-pending-deletion state))
          (marked-configmaps (kubernetes-state-marked-configmaps state))
          ((&alist 'data data
                   'metadata (&alist 'name name 'creationTimestamp created-time))
           configmap)
          (line `(line ,(concat
                         ;; Name
                         (format "%-45s " (kubernetes-utils-ellipsize name 45))

                         ;; Data
                         (propertize (format "%6s " (seq-length data)) 'face 'magit-dimmed)

                         ;; Age
                         (let ((start (apply #'encode-time (kubernetes-utils-parse-utc-timestamp created-time))))
                           (propertize (format "%6s" (kubernetes-utils-time-diff-string start current-time))
                                       'face 'magit-dimmed))))))
    `(nav-prop (:configmap-name ,name)
               (copy-prop ,name
                          ,(cond
                            ((member name pending-deletion)
                             `(propertize (face kubernetes-pending-deletion) ,line))
                            ((member name marked-configmaps)
                             `(mark-for-delete ,line))
                            (t
                             line))))))

(defun kubernetes-configmaps-render (state &optional hidden)
  (-let* (((state-set-p &as &alist 'items configmaps) (kubernetes-state-configmaps state))
          (column-heading (propertize (format "%-45s %6s %6s" "Name" "Data" "Age") 'face 'magit-section-heading)))
    `(section (configmaps-container ,hidden)
              ,(cond
                ;; If the state is set and there are no configmaps, write "None".
                ((and state-set-p (seq-empty-p configmaps))
                 `((heading ,(concat (propertize "Configmaps" 'face 'magit-header-line) " (0)"))
                   (section (configmaps-list nil)
                            (indent
                             (propertize (face magit-dimmed) (line "None."))))))

                ;; If there are configmaps, write sections for each configmaps.
                (configmaps
                 (let ((make-entry
                        (lambda (it)
                          `(section (,(intern (kubernetes-state-resource-name it)) t)
                                    (heading ,(kubernetes-configmaps--format-line state it))
                                    (section (details nil)
                                             (indent
                                              ,@(kubernetes-configmaps--format-detail it)
                                              (padding)))))))

                   `((heading ,(concat (propertize "Configmaps" 'face 'magit-header-line) " " (format "(%s)" (length configmaps))))
                     (indent
                      (line ,column-heading)
                      ,@(seq-map make-entry configmaps)))))

                ;; If there's no state, assume requests are in progress.
                (t
                 `((heading "Configmaps")
                   (indent
                    (line ,column-heading)
                    (section (configmaps-list nil)
                             (propertize (face kubernetes-progress-indicator) (line "Fetching...")))))))
              (padding))))


;; Requests and state management

(defun kubernetes-configmaps-refresh (&optional interactive)
  (unless (kubernetes-process-poll-configmaps-process-live-p)
    (kubernetes-process-set-poll-configmaps-process
     (kubernetes-kubectl-get-configmaps kubernetes-default-props
                                        (kubernetes-state)
                                        (lambda (response)
                                          (kubernetes-state-update-configmaps response)
                                          (when interactive
                                            (message "Updated configmaps.")))
                                        (lambda ()
                                          (kubernetes-process-release-poll-configmaps-process))))))

(defun kubernetes-configmaps-delete-marked (state)
  (let ((names (kubernetes-state-marked-configmaps state)))
    (dolist (name names)
      (kubernetes-state-delete-configmap name)
      (kubernetes-kubectl-delete-configmap kubernetes-default-props state name
                                           (lambda (_)
                                             (message "Deleting configmap %s succeeded." name))
                                           (lambda (_)
                                             (message "Deleting configmap %s failed" name)
                                             (kubernetes-state-mark-configmap name))))
    (kubernetes-state-trigger-redraw)))


;; Displaying configmaps.

(defun kubernetes-configmaps--redraw-configmap-buffer (configmap-name state)
  (if-let (configmap (kubernetes-state-lookup-configmap configmap-name state))
      (let ((buf (get-buffer-create kubernetes-display-configmap-buffer-name)))
        (with-current-buffer buf
          (kubernetes-display-thing-mode)
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (kubernetes-utils-json-to-yaml configmap))))
        buf)
    (error "Unknown configmap: %s" configmap-name)))

;;;###autoload
(defun kubernetes-display-configmap (configmap-name state)
  "Display information for a configmap in a new window.

STATE is the current application state.

CONFIGMAP-NAME is the name of the configmap to display."
  (interactive (list (kubernetes-utils-read-configmap-name)
                     (kubernetes-state)))
  (with-current-buffer (kubernetes-configmaps--redraw-configmap-buffer configmap-name state)
    (goto-char (point-min))
    (select-window (display-buffer (current-buffer)))))


(provide 'kubernetes-configmaps)

;;; kubernetes-configmaps.el ends here
