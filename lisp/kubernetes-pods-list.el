;;; kubernetes-pods-list.el --- Displays pods.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)

(require 'kubernetes-ast)
(require 'kubernetes-config)
(require 'kubernetes-state)
(require 'kubernetes-yaml)


;; Helper functions

(defun kubernetes-pods-list--parse-utc-timestamp (timestamp)
  "Parse TIMESTAMP string from the API into the representation used by Emacs."
  (let ((parsed (parse-time-string
                 (->> timestamp
                      (replace-regexp-in-string ":" "")
                      (replace-regexp-in-string "T" " " )
                      (replace-regexp-in-string "+" " +")))))
    (--map (or it 0) parsed)))

(defun kubernetes-pods-list--time-diff-string (start now)
  "Find the interval between START and NOW, and return a string of the coarsest unit."
  (let ((diff (time-to-seconds (time-subtract now start))))
    (car (split-string (format-seconds "%yy,%dd,%hh,%mm,%ss%z" diff) ","))))

(defun kubernetes-pods-list--pod-label (pod)
  (-let [(&alist 'metadata (&alist 'labels (&alist "name" name "job-name" job-name))) pod]
    (or name job-name)))

(defun kubernetes-pods-list--pods-for-label (label-name state)
  (let ((results (kubernetes-state-pods (kubernetes-state-empty)))
        (pods (kubernetes-state-pods state)))
    (dolist (key (hash-table-keys pods))
      (-when-let* ((pod (gethash key pods))
                   (name (kubernetes-pods-list--pod-label pod)))
        (when (equal name label-name)
          (puthash key pod results))))
    results))

(defun kubernetes-pods-list--pod-labels ()
  (-sort #'string< (-keep #'kubernetes-pods-list--pod-label (hash-table-values (kubernetes-state-pods)))))

(defun kubernetes-pods-list--sorted-keys (ht)
  (-sort (lambda (l r) (string< (symbol-name l) (symbol-name r)))
         (hash-table-keys ht)))


;; Interactive commands

(defun kubernetes-pods-list--read-label ()
  (completing-read "Label: " (kubernetes-pods-list--pod-labels) nil t nil 'kubernetes-labels))

(defun kubernetes-pods-list-display-pods-for-label (label-name state)
  "Display a list of pods in STATE with label LABEL-NAME."
  (interactive (list
                (or (get-text-property (point) 'kubernetes-label-name)
                    (kubernetes-pods-list--read-label))
                (kubernetes-state)))
  (unless label-name
    (user-error "No label name at point"))

  (with-current-buffer (get-buffer-create (format "*kubernetes-label:%s*" label-name))
    (kubernetes-mode)
    (kubernetes-ast-render (current-buffer) `(pods-for-label-list ,state ,label-name))
    (-when-let (win (display-buffer (current-buffer)))
      (select-window win))))

(defun kubernetes-pods-list-display-pod (pod-name)
  "Show the pod with string POD-NAME at point in a pop-up buffer."
  (interactive (list (get-text-property (point) 'kubernetes-pod-name)))
  (unless pod-name
    (user-error "No pod name at point"))

  (-if-let ((&hash (intern pod-name) pod) (kubernetes-state-pods))
      (when-let (win (display-buffer (kubernetes-yaml-make-buffer (format "*kubernetes-pod:%s*" pod-name) pod)))
        (select-window win))
    (user-error "Pod %s not found and may have been deleted" pod-name)))


;; Keymaps

(defconst kubernetes-pod-name-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "RET") #'kubernetes-pods-list-display-pod)
    (define-key keymap [mouse-1] #'kubernetes-pods-list-display-pod)
    keymap))

(defconst kubernetes-label-name-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "RET") #'kubernetes-pods-list-display-pods-for-label)
    (define-key keymap [mouse-1] #'kubernetes-pods-list-display-pods-for-label)
    keymap))


;; Components

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
          (time-diff
           (when started-at
             (concat (kubernetes-pods-list--time-diff-string (apply #'encode-time (kubernetes-pods-list--parse-utc-timestamp started-at))
                                         (current-time))
                     " ago")))
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

    `(section (,section-name)
              (heading (copy-prop ,name ,(concat state " " name)))
              (key-value 12 "Image" ,image)
              (key-value 12 "Restarts" ,(when restart-count (number-to-string restart-count)))
              (key-value 12 "Started" ,(when started-at `(propertize (display ,time-diff) ,started-at))))))

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

(kubernetes-ast-define-component pod-name (pod-name)
  `(propertize (keymap ,kubernetes-pod-name-map kubernetes-pod-name ,pod-name)
               (copy-prop ,pod-name ,pod-name)))

(kubernetes-ast-define-component label-name (label-name)
  `(propertize (keymap ,kubernetes-label-name-map kubernetes-label-name ,label-name)
               (section (label)
                        (key-value 12 "Label" ,label-name))))

(kubernetes-ast-define-component job-name (job-name)
  `(propertize (keymap ,kubernetes-label-name-map kubernetes-label-name ,job-name)
               (section (job)
                        (key-value 12 "Job Name" ,job-name))))

(kubernetes-ast-define-component pod (pod)
  (-let* (((&alist 'metadata (&alist 'name name
                                     'namespace namespace
                                     'labels (&alist "job-name" job-name "name" label))
                   'spec (&alist 'containers containers)
                   'status (&alist 'containerStatuses container-statuses))
           pod)
          (section-name (intern (format "pod-entry-%s" name))))

    `(section (,section-name t)
              (heading (pod-name ,name))
              (indent
               (label-name ,label)
               (job-name ,job-name)
               (section (namespace) (key-value 12 "Namespace" (namespace ,namespace)))
               (padding)
               (pod-container-list ,containers ,container-statuses))
              (padding))))

(kubernetes-ast-define-component loading-indicator ()
  `(propertize (face kubernetes-loading) "Loading..."))

(kubernetes-ast-define-component empty-pods-indicator ()
  `(propertize (face kubernetes-dimmed) "None."))

(kubernetes-ast-define-component pods-list (state &key pods updated-p)
  (let ((updated-p (or updated-p (kubernetes-state-data-received-p state)))
        (pods (or pods (kubernetes-state-pods state))))
    `(section (pods-list nil)
              (heading "Pods")
              (indent
               ,(if updated-p
                    (or (--map `(pod ,(gethash it pods)) (kubernetes-pods-list--sorted-keys pods))
                        `(empty-pods-indicator))
                  `(loading-indicator)))
              (padding))))

(kubernetes-ast-define-component pods-for-label-list (state label-name &key pods)
  (let ((pods (or pods (kubernetes-pods-list--pods-for-label label-name state))))
    `(section (pods-for-label-list)
              (label-name ,label-name)
              (padding)
              (pods-list ,state :pods ,pods))))


(provide 'kubernetes-pods-list)

;;; kubernetes-pods-list.el ends here
