;;; kubernetes-pods.el --- Rendering for Kubernetes pods  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 'seq)

(require 'kubernetes-modes)
(require 'kubernetes-props)
(require 'kubernetes-state)
(require 'kubernetes-utils)
(require 'kubernetes-yaml)

;; Component

(defun kubernetes-pods--format-detail (pod)
  (-let ((detail (lambda (k v)
                   (when v
                     `(key-value 12 ,k ,v))))

         ((&alist 'metadata (&alist 'namespace ns 'labels (&alist 'name label-name))
                  'status (&alist 'containerStatuses [(&alist 'image image 'name name)]
                                  'hostIP host-ip
                                  'podIP pod-ip
                                  'startTime start-time))
          pod))
    (list (funcall detail "Name" name)
          (funcall detail "Label" label-name)
          (funcall detail "Namespace" ns)
          (funcall detail "Image" image)
          (funcall detail "Host IP" host-ip)
          (funcall detail "Pod IP" pod-ip)
          (funcall detail "Started" start-time))))

(defun kubernetes-pods--format-line (state pod)
  (-let* ((current-time (kubernetes-state-current-time state))
          (marked-pods (kubernetes-state-marked-pods state))
          (pending-deletion (kubernetes-state-pods-pending-deletion state))
          ((&alist 'metadata (&alist 'name name)
                   'status (&alist 'containerStatuses containers
                                   'startTime start-time
                                   'phase phase))
           pod)
          ([(&alist 'restartCount restarts 'state pod-state)] containers)
          (pod-state (or (alist-get 'reason (alist-get 'waiting pod-state))
                         phase))
          (str
           (concat
            ;; Name
            (format "%-45s " (kubernetes-utils-ellipsize name 45))

            ;; State
            (let ((s (format "%-10s " (kubernetes-utils-ellipsize pod-state 10))))
              (if (equal pod-state "Running") (propertize s 'face 'magit-dimmed) s))

            ;; Count
            (format "%5s "
                    (let* ((n-ready (seq-count (-lambda ((it &as &alist 'ready r))
                                                 (eq r t))
                                               containers))
                           (count-str (format "%s/%s" n-ready (seq-length containers))))
                      (if (zerop n-ready)
                          count-str
                        (propertize count-str 'face 'magit-dimmed))))

            ;; Restarts
            (let ((s (format "%8s " restarts)))
              (cond
               ((equal 0 restarts)
                (propertize s 'face 'magit-dimmed))
               ((<= kubernetes-pod-restart-warning-threshold restarts)
                (propertize s 'face 'warning))
               (t
                s)))

            ;; Age
            (let ((start (apply #'encode-time (kubernetes-utils-parse-utc-timestamp start-time))))
              (propertize (format "%8s" (kubernetes-utils-time-diff-string start current-time))
                          'face 'magit-dimmed))))

          (str (cond
                ((member (downcase pod-state) '("running" "containercreating" "terminated"))
                 str)
                ((member (downcase pod-state) '("runcontainererror" "crashloopbackoff"))
                 (propertize str 'face 'error))
                (t
                 (propertize str 'face 'warning))))
          (line `(line ,str)))

    `(nav-prop (:pod-name ,name)
               (copy-prop ,name
                          ,(cond
                            ((member name pending-deletion)
                             `(propertize (face kubernetes-pending-deletion) ,line))
                            ((member name marked-pods)
                             `(mark-for-delete ,line))
                            (t
                             line))))))

(defun kubernetes-pods-render (state &optional hidden)
  (-let* (((pods-response &as &alist 'items pods) (kubernetes-state-pods state))
          (pods (append pods nil))
          (column-heading (propertize (format "%-45s %-10s %-5s   %6s %6s" "Name" "Status" "Ready" "Restarts" "Age")
                                      'face 'magit-section-heading)))
    `(section (pods-container ,hidden)
              ,(cond
                ;; If the state is set and there are no pods, write "None".
                ((and pods-response (null pods))
                 (let ((none (propertize "None." 'face 'magit-dimmed))
                       (heading (concat (propertize "Pods" 'face 'magit-header-line) " (0)")))
                   `((heading ,heading)
                     (section (pods-list nil)
                              (indent
                               (line ,none))))))

                ;; If there are pods, write sections for each pod.
                (pods
                 (let ((heading (concat (propertize "Pods" 'face 'magit-header-line) " " (format "(%s)" (length pods))))
                       (make-pod-entry
                        (lambda (pod)
                          `(section (,(intern (kubernetes-state-resource-name pod)) t)
                                    (heading ,(kubernetes-pods--format-line state pod))
                                    (indent
                                     (section (details nil)
                                              ,@(kubernetes-pods--format-detail pod)
                                              (padding)))))))
                   `((heading ,heading)
                     (indent
                      (line ,column-heading)
                      ,@(-map make-pod-entry pods)))))

                ;; If there's no state, assume requests are in progress.
                (t
                 (let ((fetching (propertize "Fetching..." 'face 'kubernetes-progress-indicator)))
                   `((heading "Pods")
                     (section (pods-list nil)
                              (indent
                               (line ,column-heading)
                               (line ,fetching)))))))
              (padding))))


;; Requests and state management

(defun kubernetes-pods-refresh (&optional interactive)
  (unless (kubernetes-process-poll-pods-process-live-p)
    (kubernetes-process-set-poll-pods-process
     (kubernetes-kubectl-get-pods kubernetes-props
                                  (kubernetes-state)
                                  (lambda (response)
                                    (kubernetes-state-update-pods response)
                                    (when interactive
                                      (message "Updated pods.")))
                                  (lambda ()
                                    (kubernetes-process-release-poll-pods-process))))))

(defun kubernetes-pods-delete-marked (state)
  (let ((names (kubernetes-state-marked-pods state)))
    (dolist (name names)
      (kubernetes-state-delete-pod name)
      (kubernetes-kubectl-delete-pod kubernetes-props state name
                                     (lambda (_)
                                       (message "Deleting pod %s succeeded." name))
                                     (lambda (_)
                                       (message "Deleting pod %s failed" name)
                                       (kubernetes-state-mark-pod name))))
    (kubernetes-state-trigger-redraw)))

;; Interactive commands

;;;###autoload
(defun kubernetes-display-pod (pod-name state)
  "Display information for a pod in a new window.

STATE is the current application state.

POD-NAME is the name of the pod to display."
  (interactive (let ((state (kubernetes-state)))
                 (list (kubernetes-utils-read-pod-name state) state)))
  (if-let (pod (kubernetes-state-lookup-pod pod-name state))
      (select-window
       (display-buffer
        (kubernetes-yaml-make-buffer kubernetes-pod-buffer-name pod)))
    (error "Unknown pod: %s" pod-name)))


(provide 'kubernetes-pods)

;;; kubernetes-pods.el ends here
