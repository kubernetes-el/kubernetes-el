;;; kubernetes-pods.el --- Rendering for Kubernetes pods  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 'seq)

(require 'kubernetes-ast)
(require 'kubernetes-loading-container)
(require 'kubernetes-modes)
(require 'kubernetes-props)
(require 'kubernetes-state)
(require 'kubernetes-utils)
(require 'kubernetes-yaml)


;; Components

(defconst kubernetes-pods-column-heading
  (propertize (format "%-45s %-10s %-5s   %6s %6s" "Name" "Status" "Ready" "Restarts" "Age")
              'face 'magit-section-heading))

(kubernetes-ast-define-component pod-view-detail (pod)
  (-let ((detail (lambda (k v)
                   (when v
                     `(key-value 12 ,k ,v))))

         ((&alist 'metadata (&alist 'namespace ns 'labels (&alist 'name label-name))
                  'status (&alist 'containerStatuses [(&alist 'image image 'name name)]
                                  'hostIP host-ip
                                  'podIP pod-ip
                                  'startTime start-time))
          pod))
    `(,(funcall detail "Name" name)
      ,(when label-name
         `(section (selector nil)
                   (nav-prop (:selector ,label-name)
                             ,(funcall detail "Label" (propertize label-name 'face 'kubernetes-selector)))))
      ,(when ns
         `(section (namespace nil)
                   (nav-prop (:namespace-name ,ns)
                             (key-value 12 "Namespace" ,(propertize ns 'face 'kubernetes-namespace)))))
      ,(funcall detail "Image" image)
      ,(funcall detail "Host IP" host-ip)
      ,(funcall detail "Pod IP" pod-ip)
      ,(funcall detail "Started" start-time))))

(kubernetes-ast-define-component pod-view-line (state pod)
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

(kubernetes-ast-define-component pod (state pod)
  `(section (,(intern (kubernetes-state-resource-name pod)) t)
            (heading (pod-view-line ,state ,pod))
            (indent
             (section (details nil)
                      (pod-view-detail ,pod)
                      (padding)))))

(defun kubernetes-pods--succeeded-job-pod-p (pod)
  (-let [(&alist 'status (&alist 'phase phase)) pod]
    (equal phase "Succeeded")))

(kubernetes-ast-define-component pods-list (state &optional hidden)
  (-let [(&alist 'items pods) (kubernetes-state-pods state)]
    `(section (pods-container ,hidden)
              (header-with-count "Pods" ,pods)
              (indent
               (columnar-loading-container ,pods ,kubernetes-pods-column-heading
                                           ,@(--map `(pod ,state ,it)
                                                    (-remove #'kubernetes-pods--succeeded-job-pod-p (append pods nil)))))
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
