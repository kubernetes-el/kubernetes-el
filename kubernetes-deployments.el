;;; kubernetes-deployments.el --- Rendering for Kubernetes deployments.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)

(require 'kubernetes-core)
(require 'kubernetes-kubectl)
(require 'kubernetes-modes)
(require 'kubernetes-state)
(require 'kubernetes-utils)
(require 'kubernetes-vars)
(require 'kubernetes-yaml)


;;;; Components

(defconst kubernetes-deployments--column-heading
  ["%-45s %10s %10s %10s %6s" "Name Replicas UpToDate Available Age"])

(defconst kubernetes-deployments--default-columns
  '((Name (width -45))
    (Replicas (width 10))
    (UpToDate (width 10))
    (Available (width 10))
    (Age (width 6)))
  "Possible columns to select for resource-type deployments")

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
  ;; (when (not (alist-get 'deployments-columns state))
  ;;   (setf (alist-get 'deployments-columns state) kubernetes-deployments--default-columns))
  (-let* ((current-time (kubernetes-state--get state 'current-time))
          (pending-deletion (kubernetes-state--get state 'deployments-pending-deletion))
          (marked-deployments (kubernetes-state--get state 'marked-deployments))

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
          (line
           (-let* ((row "")
                   ((&alist 'deployments-columns deployments-columns) state))
             ;; Read the formatting for the table from the kubernetes-pods--default-columns variable

             (dotimes (i (length deployments-columns))
               ;; Read the column-width (and create format-string) and header for the current column

               (let* ((col (nth i deployments-columns))
                      (col-name (car col))
                      (props (cdr col))
                      (width (car (alist-get 'width props)))
                      (fmt (concat "%" (number-to-string width) "s")))
                 ;; Depending on the value of the header we use a specific print function.
                 (setq row (concat  row (pcase  col-name
                                          ('Name
                                           (format fmt (s-truncate (abs width) name))
                                           )
                                          ('Replicas
                                           (let ((next fmt)
                                                 (str (format "%s/%s" current desired)))
                                             (cond
                                              ((zerop desired)
                                               (format fmt str))
                                              ((zerop current)
                                               (propertize (format fmt str) 'face 'warning))
                                              ((/= current desired)
                                               (format fmt str))
                                              (t
                                               (propertize (format fmt str) 'face 'kubernetes-dimmed))))
                                           )
                                          ('UpToDate
                                           (let ((next fmt))
                                             (cond
                                              ((zerop desired)
                                               (format fmt up-to-date))
                                              ((zerop up-to-date)
                                               (propertize (format fmt up-to-date) 'face 'warning))
                                              (t
                                               (propertize (format fmt up-to-date) 'face 'kubernetes-dimmed))))
                                           )
                                          ('Available
                                           (let ((next fmt))
                                             (cond
                                              ((zerop desired)
                                               (format fmt available))
                                              ((zerop available)
                                               (propertize (format fmt available) 'face 'warning))
                                              (t
                                               (propertize (format fmt available) 'face 'kubernetes-dimmed))))
                                           )
                                          ('Age
                                           (let ((start (apply #'encode-time (kubernetes-utils-parse-utc-timestamp created-time))))
                                             (propertize (format fmt (kubernetes--time-diff-string start current-time))
                                                         'face 'kubernetes-dimmed))
                                           )
                                          (_
                                           (format "%s " (format fmt "?"))
                                           ))
                                    (unless (= i (1- (length deployments-columns))) " ")))))
             row)))
    `(nav-prop (:deployment-name ,name)
               (copy-prop ,name
                          ,(cond
                            ((member name pending-deletion)
                             `(propertize (face kubernetes-pending-deletion) ,line))
                            ((member name marked-deployments)
                             `(mark-for-delete ,line))
                            ((zerop desired)
                             `(propertize (face kubernetes-dimmed) ,line))
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
  (-let* (((&alist 'deployments-columns column-settings) state)
         ((state-set-p &as &alist 'items deployments) (kubernetes-state--get state 'deployments))
         ([fmt labels] (kubernetes-utils--create-table-headers column-settings)))
    `(section (deployments-container ,hidden)
              (header-with-count "Deployments" ,deployments)
              (indent
               (columnar-loading-container ,deployments
                                           ,(propertize
                                             (apply #'format fmt (split-string labels "|"))
                                             'face
                                             'magit-section-heading)
                                           ,(--map `(deployment ,state ,it) deployments)))
              (padding))))

;; Requests and state management

(kubernetes-state-define-refreshers deployments)

(defun kubernetes-deployments-delete-marked (state)
  (let ((names (kubernetes-state--get state 'marked-deployments)))
    (dolist (name names)
      (kubernetes-state-delete-deployment name)
      (kubernetes-kubectl-delete "deployment" name state
                                 (lambda (_)
                                   (message "Deleting deployment %s succeeded." name))
                                 (lambda (_)
                                   (message "Deleting deployment %s failed" name)
                                   (kubernetes-state-mark-deployment name))))
    (kubernetes-state-trigger-redraw)))


;;;; Displaying deployments

(defun kubernetes-deployments--read-name (state)
  "Read a deployment name from the user.

STATE is the current application state.

Update the deployment state if it not set yet."
  (-let* (((&alist 'items deployments)
           (or (kubernetes-state--get state 'deployments)
               (progn
                 (message "Getting deployments...")
                 (let ((response (kubernetes-kubectl-await-on-async state (-partial #'kubernetes-kubectl-get "deployments"))))
                   (kubernetes-state-update-deployments response)
                   response))))
          (deployments (append deployments nil))
          (names (-map #'kubernetes-state-resource-name deployments)))
    (completing-read "Deployment: " names nil t)))

;;###autoload
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
