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

(defconst kubernetes-pods--column-heading
  ["%-45s %-10s %5s %10s %6s" "Name Status Ready Restarts Age"])

(kubernetes-ast-define-component pod-view-detail (pod)
  (-let* ((detail (lambda (k v)
                   (when v
                     `(key-value 12 ,k ,v))))
         ((&alist 'metadata (&alist 'namespace ns 'labels (&alist 'name label-name))
                  'status (&alist 'hostIP host-ip
                                  'containerStatuses containers
                                  'podIP pod-ip
                                  'startTime start-time))
          pod)
         ([(&alist 'image image 'name name)]
          (or containers
              (make-vector 1 (list '(name . "N/A") '(image . "N/A"))))))
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
          ([(&alist 'restartCount restarts 'state pod-state)]
           (or containers
               (make-vector 1 (list '(restartCount . 0) '(state . '(failed . '(startedAt . nil)))))))
          (start-time (or start-time (format-time-string "%Y-%m-%dT%TZ")))
          (pod-state (or (alist-get 'reason (alist-get 'waiting pod-state) phase)
                         phase))
          ([fmt] kubernetes-pods--column-heading)
          (list-fmt (split-string fmt))
          (str
           (concat
            ;; Name
            (format (pop list-fmt) (kubernetes-utils-ellipsize name 45))
            " "
            ;; Status
            (let ((s (format (pop list-fmt) (kubernetes-utils-ellipsize pod-state 10))))
              (if (equal pod-state "Running") (propertize s 'face 'magit-dimmed) s))
            " "
            ;; Ready
            (format (pop list-fmt)
                    (let* ((n-ready (seq-count (-lambda ((it &as &alist 'ready r))
                                                 (eq r t))
                                               containers))
                           (count-str (format "%s/%s" n-ready (seq-length containers))))
                      (if (zerop n-ready)
                          count-str
                        (propertize count-str 'face 'magit-dimmed))))
            " "
            ;; Restarts
            (let ((s (format (pop list-fmt) restarts)))
              (cond
               ((equal 0 restarts)
                (propertize s 'face 'magit-dimmed))
               ((<= kubernetes-pod-restart-warning-threshold restarts)
                (propertize s 'face 'warning))
               (t
                s)))
            " "
            ;; Age
            (let ((start (apply #'encode-time (kubernetes-utils-parse-utc-timestamp start-time))))
              (propertize (format (pop list-fmt) (kubernetes-utils-time-diff-string start current-time))
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
  (-let (((&alist 'items pods) (kubernetes-state-pods state))
         ([fmt labels] kubernetes-pods--column-heading))
    `(section (pods-container ,hidden)
              (header-with-count "Pods" ,pods)
              (indent
               (columnar-loading-container ,pods
                                           ,(propertize
                                             (apply #'format fmt (split-string labels))
                                             'face
                                             'magit-section-heading)
                                           ,@(--map `(pod ,state ,it)
                                                    (-remove #'kubernetes-pods--succeeded-job-pod-p (append pods nil)))))
              (padding))))

;; Requests and state management

(kubernetes-state-define-refreshers pods)

;; Displaying pods

(defun kubernetes-pods--read-name (state)
  "Read a pod name from the user.

STATE is the current application state.

Update the pod state if it not set yet."
  (-let* (((&alist 'items pods)
           (or (kubernetes-state-pods state)
               (progn
                 (message "Getting pods...")
                 (let ((response (kubernetes-kubectl-await-on-async kubernetes-props state #'kubernetes-kubectl-get-pods)))
                   (kubernetes-state-update-pods response)
                   response))))
          (pods (append pods nil))
          (names (-map #'kubernetes-state-resource-name pods)))
    (completing-read "Pod: " names nil t)))

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
