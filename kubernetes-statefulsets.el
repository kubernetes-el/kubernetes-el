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
  ["%-45s %10s %10s %10s %6s" "Name|Replicas|||Age"]
  "The two empty headers are used to align statefulsets with deployments.")

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
          (_available (or available 0))
          (_up-to-date (or up-to-date 0))
          ([fmt] kubernetes-statefulsets--column-heading)
          (list-fmt (split-string fmt))
          (line `(line ,(concat
                         ;; Name
                         (format (pop list-fmt) (kubernetes-utils-ellipsize name 45))
                         " "
                         ;; Replicas (current/desired)
                         (let ((str (format "%s/%s" current desired))
                               (next (pop list-fmt)))
                           (cond
                            ((zerop desired)
                             (format next str))
                            ((zerop current)
                             (propertize (format next str) 'face 'warning))
                            ((/= current desired)
                             (format next str))
                            (t
                             (propertize (format next str) 'face 'magit-dimmed))))
                         " "
                         ;; Up-to-date
                         (propertize (format (pop list-fmt) "") 'face 'warning)
                         " "
                         ;; Available
                         (propertize (format (pop list-fmt) "") 'face 'magit-dimmed)
                         " "
                         ;; Age
                         (let ((start (apply #'encode-time (kubernetes-utils-parse-utc-timestamp created-time))))
                           (propertize (format (pop list-fmt) (kubernetes-utils-time-diff-string start current-time))
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
  (-let (((state-set-p &as &alist 'items statefulsets)
          (kubernetes-state-statefulsets state))
         ([fmt labels] kubernetes-statefulsets--column-heading))
    `(section (statefulsets-container ,hidden)
              (header-with-count "Statefulsets" ,statefulsets)
              (indent
               (columnar-loading-container ,statefulsets
                                           ,(propertize
                                             (apply #'format fmt (split-string labels "|"))
                                             'face
                                             'magit-section-heading)
                                           ,(--map `(statefulset ,state ,it) statefulsets)))
              (padding))))

;; Requests and state management

(kubernetes-state-define-refreshers statefulsets)

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
