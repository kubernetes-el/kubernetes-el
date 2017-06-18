;;; kubernetes-pods-list.el --- Displays pods.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)

(require 'kubernetes-ast)
(require 'kubernetes-state)

(kubernetes-ast-define-component pod-container (container-spec container-status)
  (-let* (((&alist 'name name 'image image) container-spec)
          ((&alist 'state (state &as &alist
                                 'running running
                                 'terminated (terminated &as &alist 'exitCode code)
                                 'waiting waiting)
                   'restartCount restart-count)
           container-status)
          (started-at
           (car (--map (alist-get 'startedAt it)
                       (list running terminated waiting))))
          (state
           (cond
            ((null container-status)
             (propertize "Pending" 'face 'font-lock-comment-face))
            (running
             (propertize "Running" 'face 'success))
            ((and terminated (zerop code))
             (propertize (alist-get 'reason terminated) 'face 'success))
            (terminated
             (propertize (alist-get 'reason terminated) 'face 'error))
            (waiting
             (propertize (alist-get 'reason waiting) 'face 'warning))
            (t
             (message "Unknown state: %s" (prin1-to-string state))
             (propertize "Warn" 'face 'warning))))

          (section-name (intern (format "pod-container-%s" name))))

    `(section (,section-name t)
              (heading ,(concat state " " name))
              (key-value 12 "Image" ,image)
              (key-value 12 "Restarts" ,(when restart-count (number-to-string restart-count)))
              (key-value 12 "Started At" ,started-at))))

(kubernetes-ast-define-component pod-container-list (containers container-statuses)
  (when-let ((entries
              (--map (-let* (((&alist 'name name) it)
                             (status (-find (-lambda ((&alist 'name status-name))
                                              (equal name status-name))
                                            (append container-statuses nil))))
                       `(pod-container ,it ,status))
                     (append containers nil))))
    `(section (containers)
              (heading "Containers")
              (list ,@entries))))

(kubernetes-ast-define-component pod (pod)
  (-let* (((&alist 'metadata (&alist 'name name
                                     'namespace namespace
                                     'labels labels)
                   'spec (&alist 'containers containers)
                   'status (&alist 'containerStatuses container-statuses))
           pod)
          ((_ . label) (--first (equal "name" (car it)) labels))
          ((_ . job-name) (--first (equal "job-name" (car it)) labels))
          (section-name (intern (format "pod-entry-%s" name))))

    `(section (,section-name t)
              (heading ,name)
              (indent
               (section (label) (key-value 12 "Label" ,label))
               (section (job-name) (key-value 12 "Job Name" ,job-name))
               (section (namespace) (key-value 12 "Namespace" ,namespace))
               (padding)
               (pod-container-list ,containers ,container-statuses))
              (padding))))

(kubernetes-ast-define-component loading-indicator ()
  `(propertize (face kubernetes-loading) "Loading..."))

(kubernetes-ast-define-component empty-pods-indicator ()
  `(propertize (face kubernetes-dimmed) "None."))

(kubernetes-ast-define-component pods-list (state)
  (let ((updated-p (kubernetes-state-data-received-p state))
        (pods (kubernetes-state-pods state)))
    `(section (pods-list nil)
              (heading "Pods")
              (indent
               ,(if updated-p
                    (or (--map `(pod ,(gethash it pods)) (kubernetes-pods-list--sorted-keys pods))
                        `(empty-pods-indicator))
                  `(loading-indicator)))
              (padding))))

(defun kubernetes-pods-list--sorted-keys (ht)
  (-sort (lambda (l r) (string< (symbol-name l) (symbol-name r)))
         (hash-table-keys ht)))

(provide 'kubernetes-pods-list)

;;; kubernetes-pods-list.el ends here
