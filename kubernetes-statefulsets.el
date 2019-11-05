;;; kubernetes-statefulsets.el --- Rendering for Kubernetes statefulsets.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)

(require 'kubernetes-kubectl)
(require 'kubernetes-modes)
(require 'kubernetes-props)
(require 'kubernetes-state)
(require 'kubernetes-utils)
(require 'kubernetes-vars)
(require 'kubernetes-yaml)


;; Components

(defconst kubernetes-statefulsets--column-heading
  ;The two empty headers are used to align statefulsets with deployments.
  (propertize (format "%-45s %10s %10s %10s %6s" "Name" "Replicas" "" "" "Age")
              'face 'magit-section-heading))

(kubernetes-ast-define-component statefulset-detail (statefulset)
  (-let [(&alist 'metadata (&alist 'namespace ns 'creationTimestamp time)
                 'spec (&alist 'selector (&alist 'matchLabels
                                                 (&alist 'name selector-name
                                                         'component component-name)
                                                 'matchExpressions match-expressions)))
         statefulset]
    `(,(when selector-name
         `(section (selector nil)
                   (nav-prop (:selector ,selector-name)
                             (key-value 12 "Selector" ,(propertize selector-name 'face 'kubernetes-selector)))))
      ,(when component-name
         `(section (component nil)
                   (nav-prop (:component ,component-name)
                             (key-value 12 "Component" ,(propertize component-name 'face 'kubernetes-component)))))

      ,(when match-expressions
         `(section (expressions nil)
                   (heading "Match Expressions")
                   (indent ,(kubernetes-yaml-render match-expressions))))

      (section (namespace nil)
               (nav-prop (:namespace-name ,ns)
                         (key-value 12 "Namespace" ,(propertize ns 'face 'kubernetes-namespace))))
      (key-value 12 "Created" ,time))))

(kubernetes-ast-define-component statefulset-line (state statefulset)
  (-let* ((current-time (kubernetes-state-current-time state))
          (pending-deletion (kubernetes-state-statefulsets-pending-deletion state))
          (marked-statefulsets (kubernetes-state-marked-statefulsets state))

          ((&alist 'metadata (&alist 'name name 'creationTimestamp created-time)

                   'spec (&alist 'replicas desired)

                   'status (&alist 'replicas current
                                   'availableReplicas available
                                   'updatedReplicas up-to-date))
           statefulset)
          (current (or current 0))
          (desired (or desired 0))
          (available (or available 0))
          (up-to-date (or up-to-date 0))

          (line `(line ,(concat
                         ;; Name
                         (format "%-45s " (kubernetes-utils-ellipsize name 45))

                         ;; Replicas (current/desired)
                         (let ((str (format "%s/%s" current desired)))
                           (cond
                            ((zerop desired)
                             (format "%10s " str))
                            ((zerop current)
                             (propertize (format "%10s " str) 'face 'warning))
                            ((/= current desired)
                             (format "%10s " str))
                            (t
                             (propertize (format "%10s " str) 'face 'magit-dimmed))))

                         ;; Up-to-date
                         (propertize (format "%10s " "") 'face 'warning)

                         ;; Available
                         (propertize (format "%10s " "") 'face 'magit-dimmed)


                         ;; Age
                         (let ((start (apply #'encode-time (kubernetes-utils-parse-utc-timestamp created-time))))
                           (propertize (format "%6s" (kubernetes-utils-time-diff-string start current-time))
                                       'face 'magit-dimmed))))))
    `(nav-prop (:statefulset-name ,name)
               (copy-prop ,name
                          ,(cond
                            ((member name pending-deletion)
                             `(propertize (face kubernetes-pending-deletion) ,line))
                            ((member name marked-statefulsets)
                             `(mark-for-delete ,line))
                            ((zerop desired)
                             `(propertize (face magit-dimmed) ,line))
                            (t
                             line))))))

(kubernetes-ast-define-component statefulset (state statefulset)
  `(section (,(intern (kubernetes-state-resource-name statefulset)) t)
            (heading (statefulset-line ,state ,statefulset))
            (section (details nil)
                     (indent
                      (statefulset-detail ,statefulset)
                      (padding)))))

(kubernetes-ast-define-component statefulsets-list (state &optional hidden)
  (-let [(state-set-p &as &alist 'items statefulsets) (kubernetes-state-statefulsets state)]
    `(section (statefulsets-container ,hidden)
              (header-with-count "Statefulsets" ,statefulsets)
              (indent
               (columnar-loading-container ,statefulsets ,kubernetes-statefulsets--column-heading
                                           ,(--map `(statefulset ,state ,it) statefulsets)))
              (padding))))


;; Requests and state management

(defun kubernetes-statefulsets-refresh (&optional interactive)
  (unless (kubernetes-process-poll-statefulsets-process-live-p)
    (kubernetes-process-set-poll-statefulsets-process
     (kubernetes-kubectl-get-statefulsets kubernetes-props
                                         (kubernetes-state)
                                         (lambda (response)
                                           (kubernetes-state-update-statefulsets response)
                                           (when interactive
                                             (message "Updated statefulsets.")))
                                         (lambda ()
                                           (kubernetes-process-release-poll-statefulsets-process))))))

(defun kubernetes-statefulsets-delete-marked (state)
  (let ((names (kubernetes-state-marked-statefulsets state)))
    (dolist (name names)
      (kubernetes-state-delete-statefulset name)
      (kubernetes-kubectl-delete-statefulset kubernetes-props state name
                                         (lambda (_)
                                           (message "Deleting statefulset %s succeeded." name))
                                         (lambda (_)
                                           (message "Deleting statefulset %s failed" name)
                                           (kubernetes-state-mark-statefulset name))))
    (kubernetes-state-trigger-redraw)))


;; Displaying statefulsets

(defun kubernetes-statefulsets--read-name (state)
  "Read a statefulset name from the user.

STATE is the current application state.

Update the statefulset state if it not set yet."
  (-let* (((&alist 'items statefulsets)
           (or (kubernetes-state-statefulsets state)
               (progn
                 (message "Getting statefulsets...")
                 (let ((response (kubernetes-kubectl-await-on-async kubernetes-props state #'kubernetes-kubectl-get-statefulsets)))
                   (kubernetes-state-update-statefulsets response)
                   response))))
          (statefulsets (append statefulsets nil))
          (names (-map #'kubernetes-state-resource-name statefulsets)))
    (completing-read "Statefulset: " names nil t)))

;;;###autoload
(defun kubernetes-display-statefulset (statefulset-name state)
  "Display information for a statefulset in a new window.

STATE is the current application state.

STATEFULSET-NAME is the name of the statefulset to display."
  (interactive (let ((state (kubernetes-state)))
                 (list (kubernetes-statefulsets--read-name state) state)))
  (if-let (statefulset (kubernetes-state-lookup-statefulset statefulset-name state))
      (select-window
       (display-buffer
        (kubernetes-yaml-make-buffer kubernetes-display-statefulset-buffer-name statefulset)))
    (error "Unknown statefulset: %s" statefulset-name)))


(provide 'kubernetes-statefulsets)

;;; kubernetes-statefulsets.el ends here
