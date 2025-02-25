;;; kubernetes-cronjobs.el --- Rendering for Kubernetes cronjobs  -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:

(require 'dash)
(require 's)

(require 'kubernetes-ast)
(require 'kubernetes-commands)
(require 'kubernetes-core)
(require 'kubernetes-loading-container)
(require 'kubernetes-state)
(require 'kubernetes-utils)
(require 'kubernetes-vars)
(require 'kubernetes-yaml)


;; Column Heading
(defconst kubernetes-cronjobs--column-heading
  ["%-36s %-15s %-15s %7s" "Name Namespace Schedule Suspend"])

;; Component Definitions
(kubernetes-ast-define-component cronjob-detail (cronjob)
  (-let [(&alist 'metadata (&alist 'namespace ns 'creationTimestamp time 'name name)
                 'spec (&alist 'schedule schedule 'suspend suspend))
         cronjob]
    `((section (namespace nil)
               (nav-prop (:namespace-name ,ns)
                         (key-value 12 "Namespace" ,(propertize ns 'face 'kubernetes-namespace))))
      (key-value 12 "Name" ,name)
      (key-value 12 "Created" ,time)
      (key-value 12 "Schedule" ,schedule)
      (key-value 12 "Suspend" ,(if (eq suspend :json-false)
                                   "false"
                                 (propertize "true" 'face 'warning))))))

(kubernetes-ast-define-component cronjob-line (state cronjob)
  (-let* ((pending-deletion (kubernetes-state--get state 'cronjobs-pending-deletion))
          (marked-cronjobs (kubernetes-state--get state 'marked-cronjobs))
          ((&alist 'metadata (&alist 'name name 'namespace namespace)
                   'spec (&alist 'schedule schedule 'suspend suspend))
           cronjob)
          ([fmt] kubernetes-cronjobs--column-heading)
          (list-fmt  (split-string fmt))
          (line `(line ,(concat
                         ;; Name
                         (propertize (format (pop list-fmt) (s-truncate 33 name)) 'face 'default)
                         " "
                         ;; Namespace
                         (propertize (format (pop list-fmt) namespace) 'face 'kubernetes-dimmed)
                         " "
                         ;; Schedule
                         (propertize (format (pop list-fmt) schedule) 'face 'kubernetes-dimmed)
                         " "
                         ;; Suspend
                         (if (eq suspend :json-false)
                             (propertize (format (pop list-fmt) "false") 'face 'kubernetes-dimmed)
                           (propertize (format (pop list-fmt) "true") 'face 'warning))
                         ))))
    `(nav-prop (:cronjob-name ,name)
               (copy-prop ,name
                          ,(cond
                            ((member name pending-deletion)
                             `(propertize (face kubernetes-pending-deletion) ,line))
                            ((member name marked-cronjobs)
                             `(mark-for-delete ,line))
                            (t
                             line))))))


(kubernetes-ast-define-component cronjob (state cronjob)
  `(section (,(intern (kubernetes-state-resource-name cronjob)) t)
            (heading (cronjob-line ,state ,cronjob))
            (indent
             (cronjob-detail ,cronjob)
             (padding))))

(kubernetes-ast-define-component cronjobs-list (state &optional hidden)
  (-let (((&alist 'items cronjobs) (kubernetes-state--get state 'cronjobs))
         ([fmt labels] kubernetes-cronjobs--column-heading))
    `(section (cronjobs-container ,hidden)
              (header-with-count "CronJobs" ,cronjobs)

              (indent
               (columnar-loading-container ,cronjobs
                                           ,(propertize
                                             (apply #'format fmt (split-string labels))
                                             'face
                                             'magit-section-heading)
                                           ,(--map `(cronjob ,state ,it) cronjobs)))
              (padding))))

;; Requests and State Management

(kubernetes-state-define-refreshers cronjobs)

(defun kubernetes-cronjobs-delete-marked (state)
  "Delete marked cronjobs.

STATE is the current application state."
  (let ((names (kubernetes-state--get state 'marked-cronjobs)))
    (dolist (name names)
      (kubernetes-state-delete-cronjob name)
      (kubernetes-kubectl-delete "cronjob" name state
                                 (lambda (_)
                                   (message "Deleting CronJob %s succeeded." name))
                                 (lambda (_)
                                   (message "Deleting CronJob %s failed" name)
                                   (kubernetes-state-mark-cronjob name))))
    (kubernetes-state-trigger-redraw)))

;; Displaying CronJobs

(defun kubernetes-cronjobs--read-name (state)
  "Read a cronjob name from the user.
STATE is the current application state."
  (-let* (((&alist 'items cronjobs)
           (or (kubernetes-state--get state 'cronjobs)
               (progn
                 (message "Getting cronjobs...")
                 (let ((response (kubernetes-kubectl-await-on-async state (-partial #'kubernetes-kubectl-get "cronjobs"))))
                   (kubernetes-state-update-cronjobs response)
                   response))))
          (names (-map #'kubernetes-state-resource-name cronjobs)))
    (completing-read "CronJob: " names nil t)))

(defun kubernetes-display-cronjob (cronjob-name state)
  "Display information for a cronjob in a new window.
STATE is the current application state.
CRONJOB-NAME is the name of the cronjob to display."
  (interactive (let ((state (kubernetes-state)))
                 (list (kubernetes-cronjobs--read-name state) state)))
  (if-let (cronjob (kubernetes-state-lookup-cronjob cronjob-name state))
      (select-window
       (display-buffer
        (kubernetes-yaml-make-buffer "*Kubernetes CronJob*" cronjob)))
    (error "Unknown CronJob: %s" cronjob-name)))

(provide 'kubernetes-cronjobs)

;;; kubernetes-cronjobs.el ends here
