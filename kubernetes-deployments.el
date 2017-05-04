;;; kubernetes-deployments.el --- Rendering for Kubernetes deployments.  -*- lexical-binding: t; -*-
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

(defconst kubernetes-deployments--column-heading
  (propertize (format "%-45s %10s %10s %10s %6s" "Name" "Replicas" "UpToDate" "Available" "Age")
              'face 'magit-section-heading))

(kubernetes-ast-define-component deployment-detail (deployment)
  (-let [(&alist 'metadata (&alist 'namespace ns 'creationTimestamp time)
                 'spec (&alist 'selector (&alist 'matchLabels
                                                 (&alist 'name selector-name
                                                         'component component-name)
                                                 'matchExpressions match-expressions)))
         deployment]
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

(kubernetes-ast-define-component deployment-line (state deployment)
  (-let* ((current-time (kubernetes-state-current-time state))
          (pending-deletion (kubernetes-state-deployments-pending-deletion state))
          (marked-deployments (kubernetes-state-marked-deployments state))

          ((&alist 'metadata (&alist 'name name 'creationTimestamp created-time)

                   'spec (&alist 'replicas desired)

                   'status (&alist 'replicas current
                                   'availableReplicas available
                                   'updatedReplicas up-to-date))
           deployment)
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
    `(nav-prop (:deployment-name ,name)
               (copy-prop ,name
                          ,(cond
                            ((member name pending-deletion)
                             `(propertize (face kubernetes-pending-deletion) ,line))
                            ((member name marked-deployments)
                             `(mark-for-delete ,line))
                            ((zerop desired)
                             `(propertize (face magit-dimmed) ,line))
                            (t
                             line))))))

(kubernetes-ast-define-component deployment (state deployment)
  `(section (,(intern (kubernetes-state-resource-name deployment)) t)
            (heading (deployment-line ,state ,deployment))
            (section (details nil)
                     (indent
                      (deployment-detail ,deployment)
                      (padding)))))

(kubernetes-ast-define-component deployments-list (state &optional hidden)
  (-let [(state-set-p &as &alist 'items deployments) (kubernetes-state-deployments state)]
    `(section (deployments-container ,hidden)
              (header-with-count "Deployments" ,deployments)
              (indent
               (columnar-loading-container ,deployments ,kubernetes-deployments--column-heading
                                           ,(--map `(deployment ,state ,it) deployments)))
              (padding))))


;; Requests and state management

(defun kubernetes-deployments-refresh (&optional interactive)
  (unless (kubernetes-process-poll-deployments-process-live-p)
    (kubernetes-process-set-poll-deployments-process
     (kubernetes-kubectl-get-deployments kubernetes-props
                                         (kubernetes-state)
                                         (lambda (response)
                                           (kubernetes-state-update-deployments response)
                                           (when interactive
                                             (message "Updated deployments.")))
                                         (lambda ()
                                           (kubernetes-process-release-poll-deployments-process))))))

(defun kubernetes-deployments-delete-marked (state)
  (let ((names (kubernetes-state-marked-deployments state)))
    (dolist (name names)
      (kubernetes-state-delete-deployment name)
      (kubernetes-kubectl-delete-deployment kubernetes-props state name
                                         (lambda (_)
                                           (message "Deleting deployment %s succeeded." name))
                                         (lambda (_)
                                           (message "Deleting deployment %s failed" name)
                                           (kubernetes-state-mark-deployment name))))
    (kubernetes-state-trigger-redraw)))


;; Displaying deployments

(defun kubernetes-deployments--read-name (state)
  "Read a deployment name from the user.

STATE is the current application state.

Update the deployment state if it not set yet."
  (-let* (((&alist 'items deployments)
           (or (kubernetes-state-deployments state)
               (progn
                 (message "Getting deployments...")
                 (let ((response (kubernetes-kubectl-await-on-async kubernetes-props state #'kubernetes-kubectl-get-deployments)))
                   (kubernetes-state-update-deployments response)
                   response))))
          (deployments (append deployments nil))
          (names (-map #'kubernetes-state-resource-name deployments)))
    (completing-read "Deployment: " names nil t)))

;;;###autoload
(defun kubernetes-display-deployment (deployment-name state)
  "Display information for a deployment in a new window.

STATE is the current application state.

DEPLOYMENT-NAME is the name of the deployment to display."
  (interactive (let ((state (kubernetes-state)))
                 (list (kubernetes-deployments--read-name state) state)))
  (if-let (deployment (kubernetes-state-lookup-deployment deployment-name state))
      (select-window
       (display-buffer
        (kubernetes-yaml-make-buffer kubernetes-display-deployment-buffer-name deployment)))
    (error "Unknown deployment: %s" deployment-name)))


(provide 'kubernetes-deployments)

;;; kubernetes-deployments.el ends here
