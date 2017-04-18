;;; kubernetes-configmaps.el --- Rendering for Kubernetes configmaps.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)

(require 'kubernetes-modes)
(require 'kubernetes-process)
(require 'kubernetes-props)
(require 'kubernetes-state)
(require 'kubernetes-utils)
(require 'kubernetes-yaml)


;; Component

(defconst kubernetes-configmaps--column-heading
  (propertize (format "%-45s %6s %6s" "Name" "Data" "Age")
              'face 'magit-section-heading))

(defun kubernetes-configmaps--format-detail (configmap)
  (-let [(&alist 'metadata (&alist 'namespace ns 'creationTimestamp time)) configmap]
    `((section (namespace nil)
               (nav-prop (:namespace-name ,ns)
                         (key-value 12 "Namespace" ,(propertize ns 'face 'kubernetes-namespace))))
      (key-value 12 "Created" ,time))))

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

(defun kubernetes-configmaps-render-configmap (state configmap)
  `(section (,(intern (kubernetes-state-resource-name configmap)) t)
            (heading ,(kubernetes-configmaps--format-line state configmap))
            (section (details nil)
                     (indent
                      ,@(kubernetes-configmaps--format-detail configmap)
                      (padding)))))

(defun kubernetes-configmaps-render (state &optional hidden)
  (-let [(state-set-p &as &alist 'items configmaps) (kubernetes-state-configmaps state)]
    `(section (configmaps-container ,hidden)
              ,(cond
                ;; If the state is set and there are no configmaps, write "None".
                ((and state-set-p (seq-empty-p configmaps))
                 `((heading ,(concat (propertize "Configmaps" 'face 'magit-header-line) " (0)"))
                   (section (configmaps-list nil)
                            (indent
                             (propertize (face magit-dimmed) (line "None."))))))

                ;; If there are configmaps, write sections for each configmap.
                (configmaps
                 `((heading ,(concat (propertize "Configmaps" 'face 'magit-header-line) " " (format "(%s)" (length configmaps))))
                   (indent
                    (line ,kubernetes-configmaps--column-heading)
                    ,@(seq-map (lambda (it) (kubernetes-configmaps-render-configmap state it)) configmaps))))

                ;; If there's no state, assume requests are in progress.
                (t
                 `((heading "Configmaps")
                   (indent
                    (line ,kubernetes-configmaps--column-heading)
                    (section (configmaps-list nil)
                             (propertize (face kubernetes-progress-indicator) (line "Fetching...")))))))
              (padding))))


;; Requests and state management

(defun kubernetes-configmaps-refresh (&optional interactive)
  (unless (kubernetes-process-poll-configmaps-process-live-p)
    (kubernetes-process-set-poll-configmaps-process
     (kubernetes-kubectl-get-configmaps kubernetes-props
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
      (kubernetes-kubectl-delete-configmap kubernetes-props state name
                                           (lambda (_)
                                             (message "Deleting configmap %s succeeded." name))
                                           (lambda (_)
                                             (message "Deleting configmap %s failed" name)
                                             (kubernetes-state-mark-configmap name))))
    (kubernetes-state-trigger-redraw)))


;; Displaying configmaps.

(defun kubernetes-configmaps--read-name (state)
  "Read a configmap name from the user.

STATE is the current application state.

Update the configmap state if it not set yet."
  (-let* (((&alist 'items configmaps)
           (or (kubernetes-state-configmaps state)
               (progn
                 (message "Getting configmaps...")
                 (let ((response (kubernetes-kubectl-await-on-async kubernetes-props state #'kubernetes-kubectl-get-configmaps)))
                   (kubernetes-state-update-configmaps response)
                   response))))
          (configmaps (append configmaps nil))
          (names (-map #'kubernetes-state-resource-name configmaps)))
    (completing-read "Configmap: " names nil t)))


;;;###autoload
(defun kubernetes-display-configmap (configmap-name state)
  "Display information for a configmap in a new window.

STATE is the current application state.

CONFIGMAP-NAME is the name of the configmap to display."
  (interactive (let ((state (kubernetes-state)))
                 (list (kubernetes-configmaps--read-name state) state)))
  (if-let (configmap (kubernetes-state-lookup-configmap configmap-name state))
      (select-window
       (display-buffer
        (kubernetes-yaml-make-buffer kubernetes-display-configmap-buffer-name configmap)))
    (error "Unknown configmap: %s" configmap-name)))


(provide 'kubernetes-configmaps)

;;; kubernetes-configmaps.el ends here
