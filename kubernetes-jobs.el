;;; kubernetes-jobs.el --- Rendering for Kubernetes jobs.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)

(require 'kubernetes-ast)
(require 'kubernetes-core)
(require 'kubernetes-kubectl)
(require 'kubernetes-modes)
(require 'kubernetes-pod-line)
(require 'kubernetes-state)
(require 'kubernetes-utils)
(require 'kubernetes-vars)
(require 'kubernetes-yaml)


;; Components

(defconst kubernetes-jobs--column-heading
  ["%-45s %10s %6s" "Name Successful Age"])

(kubernetes-ast-define-component job-detail (state pod job)
  (-let* (((&alist 'metadata (&alist 'namespace ns
                                     'creationTimestamp time)
                   'spec (&alist 'template
                                 (&alist 'spec (&alist 'restartPolicy restart-policy)))
                   'status (&alist
                            'startTime start-time
                            'completionTime completion-time))
           job)
          ((&alist 'items pods) (kubernetes-state--get state 'pods)))

    `((section (namespace nil)
               (nav-prop (:namespace-name ,ns)
                         (key-value 12 "Namespace" ,(propertize ns 'face 'kubernetes-namespace))))
      ,(when restart-policy
         `(key-value 12 "RestartPolicy" ,restart-policy))
      (padding)

      (key-value 12 "Created" ,time)
      ,(when start-time
         `(key-value 12 "Started" ,start-time))
      ,(when completion-time
         `(key-value 12 "Completed" ,completion-time))
      (padding)

      (section (pod nil)
               (heading "Pod")
               (indent
                (membership-loading-container ,pod ,pods
                                              (pod-line ,state ,pod)
                                              (padding)))))))

(kubernetes-ast-define-component job-line (state pod job)
  (-let* ((current-time (kubernetes-state-current-time state))
          (pending-deletion (kubernetes-state--get state 'jobs-pending-deletion))
          (marked-jobs (kubernetes-state--get state 'marked-jobs))

          ((&alist 'metadata (&alist 'name name 'creationTimestamp created-time)
                   'status (&alist 'succeeded successful
                                   'completionTime completion-time))
           job)
          (successful (or successful 0))
          ([fmt] kubernetes-jobs--column-heading)
          (list-fmt (split-string fmt))
          (line (concat
                 ;; Name
                 (let ((name-str (format (pop list-fmt) (s-truncate 43 name))))
                   (cond
                    ((and completion-time (< 0 successful))
                     (propertize name-str 'face 'kubernetes-dimmed))
                    ((not (kubernetes-pod-line-ok-p pod))
                     (propertize name-str 'face 'warning))
                    (t
                     name-str)))
                 " "
                 ;; Successful
                 (propertize (format (pop list-fmt) successful) 'face 'kubernetes-dimmed)
                 " "
                 ;; Age
                 (let ((start (apply #'encode-time (kubernetes-utils-parse-utc-timestamp created-time))))
                   (propertize (format (pop list-fmt) (kubernetes--time-diff-string start current-time))
                               'face 'kubernetes-dimmed)))))

    `(nav-prop (:job-name ,name)
               (copy-prop ,name
                          (line ,(cond
                                  ((member name pending-deletion)
                                   `(propertize (face kubernetes-pending-deletion) ,line))
                                  ((member name marked-jobs)
                                   `(mark-for-delete ,line))
                                  (t
                                   line)))))))

(defun kubernetes-jobs--lookup-pod-for-job (job state)
  "Find a pod for JOB in STATE.
This function finds pods by matching the job's UID with pod's ownerReferences."
  (let* ((job-name (kubernetes-state-resource-name job))
         (job-uid (cdr (assoc 'uid (cdr (assoc 'metadata job)))))
         (pod-items (cdr (assoc 'items (kubernetes-state--get state 'pods)))))
    (when (and job-uid pod-items)
      (seq-find
       (lambda (pod)
         (let* ((owner-refs (cdr (assoc 'ownerReferences (cdr (assoc 'metadata pod))))))
           (and owner-refs
                (seq-find (lambda (ref)
                            (and (string= (cdr (assoc 'kind ref)) "Job")
                                 (string= (cdr (assoc 'uid ref)) job-uid)))
                          owner-refs))))
       pod-items))))


(kubernetes-ast-define-component job (state job)
  (let ((pod (kubernetes-jobs--lookup-pod-for-job job state)))
    `(section (,(intern (kubernetes-state-resource-name job)) t)
              (heading (job-line ,state ,pod ,job))
              (section (details nil)
                       (indent
                        (job-detail ,state ,pod ,job))))))

(kubernetes-ast-define-component jobs-list (state &optional hidden)
  (-let (((state-set-p &as &alist 'items jobs) (kubernetes-state--get state 'jobs))
         ([fmt labels] kubernetes-jobs--column-heading))
    `(section (jobs-container ,hidden)
              (header-with-count "Jobs" ,jobs)
              (indent
               (columnar-loading-container ,jobs
                                           ,(propertize
                                             (apply #'format fmt (split-string labels))
                                             'face
                                             'magit-section-heading)
                                           ,(--map `(job ,state ,it) jobs)))
              (padding))))

;; Requests and state management

(kubernetes-state-define-refreshers jobs)

(defun kubernetes-jobs-delete-marked (state)
  (let ((names (kubernetes-state--get state 'marked-jobs)))
    (dolist (name names)
      (kubernetes-state-delete-job name)
      (kubernetes-kubectl-delete "job" name state
                                 (lambda (_)
                                   (message "Deleting job %s succeeded." name))
                                 (lambda (_)
                                   (message "Deleting job %s failed" name)
                                   (kubernetes-state-mark-job name))))
    (kubernetes-state-trigger-redraw)))


;; Displaying jobs

(defun kubernetes-jobs--read-name (state)
  "Read a job name from the user.

STATE is the current application state.

Update the job state if it not set yet."
  (-let* (((&alist 'items jobs)
           (or (kubernetes-state--get state 'jobs)
               (progn
                 (message "Getting jobs...")
                 (let ((response (kubernetes-kubectl-await-on-async state (-partial #'kubernetes-kubectl-get "jobs"))))
                   (kubernetes-state-update-jobs response)
                   response))))
          (jobs (append jobs nil))
          (names (-map #'kubernetes-state-resource-name jobs)))
    (completing-read "Job: " names nil t)))

;;;###autoload
(defun kubernetes-display-job (job-name state)
  "Display information for a job in a new window.

STATE is the current application state.

JOB-NAME is the name of the job to display."
       (interactive (let ((state (kubernetes-state)))
                      (list (kubernetes-jobs--read-name state) state)))
       (if-let (job (kubernetes-state-lookup-job job-name state))
           (select-window
            (display-buffer
             (kubernetes-yaml-make-buffer kubernetes-display-job-buffer-name job)))
         (error "Unknown job: %s" job-name)))


(provide 'kubernetes-jobs)

;;; kubernetes-jobs.el ends here
