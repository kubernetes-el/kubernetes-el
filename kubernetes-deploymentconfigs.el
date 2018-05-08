;;; kubernetes-deploymentconfigs.el --- Rendering for Kubernetes deploymentconfigs.  -*- lexical-binding: t; -*-
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

(defconst kubernetes-deploymentconfigs--column-heading
  ;The two empty headers are used to align deploymentconfigs with deployments.
  (propertize (format "%-45s %10s %10s %10s %6s" "Name" "Replicas" "UpToDate" "Available" "Age")
              'face 'magit-section-heading))

(kubernetes-ast-define-component deploymentconfig-detail (deploymentconfig)
  (-let [(&alist 'metadata (&alist 'namespace ns 'creationTimestamp time)
                 'spec (&alist 'selector (&alist 'matchLabels
                                                 (&alist 'name selector-name
                                                         'component component-name)
                                                 'matchExpressions match-expressions)))
         deploymentconfig]
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

(kubernetes-ast-define-component deploymentconfig-line (state deploymentconfig)
  (-let* ((current-time (kubernetes-state-current-time state))
          (pending-deletion (kubernetes-state-deploymentconfigs-pending-deletion state))
          (marked-deploymentconfigs (kubernetes-state-marked-deploymentconfigs state))

          ((&alist 'metadata (&alist 'name name 'creationTimestamp created-time)

                   'spec (&alist 'replicas desired)

                   'status (&alist 'replicas current
                                   'availableReplicas available
                                   'updatedReplicas up-to-date))
           deploymentconfig)
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
                         (cond
                          ((zerop desired)
                           (format "%10s " up-to-date))
                          ((zerop up-to-date)
                           (propertize (format "%10s " up-to-date) 'face 'warning))
                          (t
                           (propertize (format "%10s " up-to-date) 'face 'magit-dimmed)))

                         ;; Available
                         (cond
                          ((zerop desired)
                           (format "%10s " available))
                          ((zerop available)
                           (propertize (format "%10s " available) 'face 'warning))
                          (t
                           (propertize (format "%10s " available) 'face 'magit-dimmed)))

                         ;; Age
                         (let ((start (apply #'encode-time (kubernetes-utils-parse-utc-timestamp created-time))))
                           (propertize (format "%6s" (kubernetes-utils-time-diff-string start current-time))
                                       'face 'magit-dimmed))))))
    `(nav-prop (:deploymentconfig-name ,name)
               (copy-prop ,name
                          ,(cond
                            ((member name pending-deletion)
                             `(propertize (face kubernetes-pending-deletion) ,line))
                            ((member name marked-deploymentconfigs)
                             `(mark-for-delete ,line))
                            ((zerop desired)
                             `(propertize (face magit-dimmed) ,line))
                            (t
                             line))))))

(kubernetes-ast-define-component deploymentconfig (state deploymentconfig)
  `(section (,(intern (kubernetes-state-resource-name deploymentconfig)) t)
            (heading (deploymentconfig-line ,state ,deploymentconfig))
            (section (details nil)
                     (indent
                      (deploymentconfig-detail ,deploymentconfig)
                      (padding)))))

(kubernetes-ast-define-component deploymentconfigs-list (state &optional hidden)
  (-let [(state-set-p &as &alist 'items deploymentconfigs) (kubernetes-state-deploymentconfigs state)]
    `(section (deploymentconfigs-container ,hidden)
              (header-with-count "Deploymentconfigs" ,deploymentconfigs)
              (indent
               (columnar-loading-container ,deploymentconfigs ,kubernetes-deploymentconfigs--column-heading
                                           ,(--map `(deploymentconfig ,state ,it) deploymentconfigs)))
              (padding))))


;; Requests and state management

(defun kubernetes-deploymentconfigs-refresh (&optional interactive)
  (unless (kubernetes-process-poll-deploymentconfigs-process-live-p)
    (kubernetes-process-set-poll-deploymentconfigs-process
     (kubernetes-kubectl-get-deploymentconfigs kubernetes-props
                                         (kubernetes-state)
                                         (lambda (response)
                                           (kubernetes-state-update-deploymentconfigs response)
                                           (when interactive
                                             (message "Updated deploymentconfigs.")))
                                         (lambda ()
                                           (kubernetes-process-release-poll-deploymentconfigs-process))))))

(defun kubernetes-deploymentconfigs-delete-marked (state)
  (let ((names (kubernetes-state-marked-deploymentconfigs state)))
    (dolist (name names)
      (kubernetes-state-delete-deploymentconfig name)
      (kubernetes-kubectl-delete-deploymentconfig kubernetes-props state name
                                         (lambda (_)
                                           (message "Deleting deploymentconfig %s succeeded." name))
                                         (lambda (_)
                                           (message "Deleting deploymentconfig %s failed" name)
                                           (kubernetes-state-mark-deploymentconfig name))))
    (kubernetes-state-trigger-redraw)))


;; Displaying deploymentconfigs

(defun kubernetes-deploymentconfigs--read-name (state)
  "Read a deploymentconfig name from the user.

STATE is the current application state.

Update the deploymentconfig state if it not set yet."
  (-let* (((&alist 'items deploymentconfigs)
           (or (kubernetes-state-deploymentconfigs state)
               (progn
                 (message "Getting deploymentconfigs...")
                 (let ((response (kubernetes-kubectl-await-on-async kubernetes-props state #'kubernetes-kubectl-get-deploymentconfigs)))
                   (kubernetes-state-update-deploymentconfigs response)
                   response))))
          (deploymentconfigs (append deploymentconfigs nil))
          (names (-map #'kubernetes-state-resource-name deploymentconfigs)))
    (completing-read "Deploymentconfig: " names nil t)))

;;;###autoload
(defun kubernetes-display-deploymentconfig (deploymentconfig-name state)
  "Display information for a deploymentconfig in a new window.

STATE is the current application state.

DEPLOYMENTCONFIG-NAME is the name of the deploymentconfig to display."
  (interactive (let ((state (kubernetes-state)))
                 (list (kubernetes-deploymentconfigs--read-name state) state)))
  (if-let (deploymentconfig (kubernetes-state-lookup-deploymentconfig deploymentconfig-name state))
      (select-window
       (display-buffer
        (kubernetes-yaml-make-buffer kubernetes-display-deploymentconfig-buffer-name deploymentconfig)))
    (error "Unknown deploymentconfig: %s" deploymentconfig-name)))


(provide 'kubernetes-deploymentconfigs)

;;; kubernetes-deploymentconfigs.el ends here
