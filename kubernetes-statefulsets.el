;;; kubernetes-statefulsets.el --- Rendering for Kubernetes statefulsets.  -*- lexical-binding: t; -*-
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


;; Components

(defconst kubernetes-statefulsets--column-heading
  ["%-45s %10s %10s %10s %6s" "Name|Replicas|||Age"]
  "The two empty headers are used to align statefulsets with deployments.")

(defconst kubernetes-statefulsets--default-columns
  ;; TODO The two empty headers are used to align statefulsets with deployments.
  '((Name (width -45))
    (Replicas (width 10))
    (- (width 10))
    (- (width 10))
    (Age (width 6)))
  "Possible columns to select for resource-type statefulsets")

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
  ;; (when (not (alist-get 'statefulsets-columns state))
  ;;   (setf (alist-get 'statefulsets-columns state) kubernetes-statefulsets--default-columns))
  (-let* ((current-time (kubernetes-state--get state 'current-time))
          (pending-deletion (kubernetes-state--get state 'statefulsets-pending-deletion))
          (marked-statefulsets (kubernetes-state--get state 'marked-statefulsets))
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
          (line
           (-let* ((row "")
                   ((&alist 'statefulsets-columns statefulsets-columns) state))
             ;; Read the formatting for the table from the kubernetes-pods--default-columns variable
             (dotimes (i (length statefulsets-columns))
               ;; Read the column-width (and create format-string) and header for the current column
               (let* ((col (nth i statefulsets-columns))
                      (col-name (car col))
                      (props (cdr col))
                      (width (car (alist-get 'width props)))
                      (fmt (concat "%" (number-to-string width) "s")))
                 ;; Depending on the value of the header we use a specific print function.
                 (setq row (concat row (pcase  col-name
                                         ('Name
                                          (format fmt (s-truncate (abs width) name))
                                          )
                                         ('Replicas
                                          (let ((str (format "%s/%s" current desired))
                                                ;; TODO remove next, use fmt directly
                                                (next fmt))
                                            (cond
                                             ((zerop desired)
                                              (format next str))
                                             ((zerop current)
                                              (propertize (format next str) 'face 'warning))
                                             ((/= current desired)
                                              (format next str))
                                             (t
                                              (propertize (format next str) 'face 'kubernetes-dimmed)))))
                                         ('Age
                                          (let ((start (apply #'encode-time (kubernetes-utils-parse-utc-timestamp created-time))))
                                            (propertize (format fmt (kubernetes--time-diff-string start current-time))
                                                        'face 'kubernetes-dimmed)))
                                         (_
                                          (format "%s" (format fmt ""))
                                          ))
                                   (unless (= i (1- (length statefulsets-columns))) " ")))))
             row)))
    `(nav-prop (:statefulset-name ,name)
               (copy-prop ,name
                          ,(cond
                            ((member name pending-deletion)
                             `(propertize (face kubernetes-pending-deletion) ,line))
                            ((member name marked-statefulsets)
                             `(mark-for-delete ,line))
                            ((zerop desired)
                             `(propertize (face kubernetes-dimmed) ,line))
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
  (-let* (((&alist 'statefulsets-columns column-settings) state)
         ((state-set-p &as &alist 'items statefulsets)
          (kubernetes-state--get state 'statefulsets))
         ([fmt labels] (kubernetes-utils--create-table-headers column-settings)))
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
  (let ((names (kubernetes-state--get state 'marked-statefulsets)))
    (dolist (name names)
      (kubernetes-state-delete-statefulset name)
      (kubernetes-kubectl-delete "statefulset" name state
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
           (or (kubernetes-state--get state 'statefulsets)
               (progn
                 (message "Getting statefulsets...")
                 (let ((response (kubernetes-kubectl-await-on-async state (-partial #'kubernetes-kubectl-get "statefulsets"))))
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
