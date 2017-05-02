;;; kubernetes-jobs.el --- Rendering for Kubernetes jobs.  -*- lexical-binding: t; -*-
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


;; Component

(defconst kubernetes-jobs--column-heading
  (propertize (format "%-45s %10s %6s" "Name" "Successful" "Age")
              'face 'magit-section-heading))

(defun kubernetes-jobs--format-container (container)
  (-let [(&alist 'image image 'name name) container]
    (when name
      `(section (container nil)
                (section (selector nil)
                         (nav-prop (:selector ,name)
                                   (key-value 12 "Name" ,(propertize name 'face 'kubernetes-selector))))
                ,(when image
                   `(key-value 12 "Image" ,image))
                (padding)))))

(defun kubernetes-jobs--format-detail (job)
  (-let [(&alist 'metadata (&alist 'namespace ns 'creationTimestamp time)
                 'spec (&alist 'template
                               (&alist 'spec (&alist 'containers containers
                                                     'restartPolicy restart-policy)))
                 'status (&alist
                          'startTime start-time
                          'completionTime completion-time))
         job]
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

      ,(when-let (cs (seq-map #'kubernetes-jobs--format-container (append containers nil)))
         `(section (containers nil)
                   (heading "Containers")
                   (indent
                    ,cs))))))

(defun kubernetes-jobs--format-line (state job)
  (-let* ((current-time (kubernetes-state-current-time state))
          (pending-deletion (kubernetes-state-jobs-pending-deletion state))
          (marked-jobs (kubernetes-state-marked-jobs state))

          ((&alist 'metadata (&alist 'name name 'creationTimestamp created-time)
                   'status (&alist 'succeeded successful
                                   'completionTime completion-time))
           job)

          (successful (or successful 0))

          (line (concat
                 ;; Name
                 (let ((name-str (format "%-45s " (kubernetes-utils-ellipsize name 45))))
                   (cond
                    ((and completion-time (< 0 successful))
                     (propertize name-str 'face 'magit-dimmed))
                    ((zerop successful)
                     (propertize name-str 'face 'warning))
                    (t
                     name-str)))
                 ;; Successful
                 (propertize (format "%8s   " successful) 'face 'magit-dimmed)
                 ;; Age
                 (let ((start (apply #'encode-time (kubernetes-utils-parse-utc-timestamp created-time))))
                   (propertize (format "%6s" (kubernetes-utils-time-diff-string start current-time))
                               'face 'magit-dimmed)))))

    `(nav-prop (:job-name ,name)
               (copy-prop ,name
                          (line ,(cond
                                  ((member name pending-deletion)
                                   `(propertize (face kubernetes-pending-deletion) ,line))
                                  ((member name marked-jobs)
                                   `(mark-for-delete ,line))
                                  (t
                                   line)))))))

(defun kubernetes-jobs-render-job (state job)
  `(section (,(intern (kubernetes-state-resource-name job)) t)
            (heading ,(kubernetes-jobs--format-line state job))
            (section (details nil)
                     (indent
                      ,@(kubernetes-jobs--format-detail job)))))

(defun kubernetes-jobs-render (state &optional hidden)
  (-let [(state-set-p &as &alist 'items jobs) (kubernetes-state-jobs state)]
    `(section (jobs-container ,hidden)
              ,(cond
                ;; If the state is set and there are no jobs, write "None".
                ((and state-set-p (null (append jobs nil)))
                 `((heading ,(concat (propertize "Jobs" 'face 'magit-header-line) " (0)"))
                   (section (jobs-list nil)
                            (indent
                             (propertize (face magit-dimmed) (line "None."))))))

                ;; If there are jobs, write sections for each job.
                (jobs
                 `((heading ,(concat (propertize "Jobs" 'face 'magit-header-line) " " (format "(%s)" (length jobs))))
                   (indent
                    (line ,kubernetes-jobs--column-heading)
                    ,@(seq-map (lambda (it) (kubernetes-jobs-render-job state it)) jobs))))

                ;; If there's no state, assume requests are in progress.
                (t
                 `((heading "Jobs")
                   (indent
                    (line ,kubernetes-jobs--column-heading)
                    (section (jobs-list nil)
                             (propertize (face kubernetes-progress-indicator) (line "Fetching...")))))))
              (padding))))


;; Requests and state management

(defun kubernetes-jobs-refresh (&optional interactive)
  (unless (kubernetes-process-poll-jobs-process-live-p)
    (kubernetes-process-set-poll-jobs-process
     (kubernetes-kubectl-get-jobs kubernetes-props
                                  (kubernetes-state)
                                  (lambda (response)
                                    (kubernetes-state-update-jobs response)
                                    (when interactive
                                      (message "Updated jobs.")))
                                  (lambda ()
                                    (kubernetes-process-release-poll-jobs-process))))))

(defun kubernetes-jobs-delete-marked (state)
  (let ((names (kubernetes-state-marked-jobs state)))
    (dolist (name names)
      (kubernetes-state-delete-job name)
      (kubernetes-kubectl-delete-job kubernetes-props state name
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
           (or (kubernetes-state-jobs state)
               (progn
                 (message "Getting jobs...")
                 (let ((response (kubernetes-kubectl-await-on-async kubernetes-props state #'kubernetes-kubectl-get-jobs)))
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
