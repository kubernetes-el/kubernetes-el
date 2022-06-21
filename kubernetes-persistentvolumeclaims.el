;;; kubernetes-persistentvolumeclaims.el --- Rendering for Kubernetes persistent volume claims.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)

(require 'kubernetes-ast)
(require 'kubernetes-core)
(require 'kubernetes-loading-container)
(require 'kubernetes-process)
(require 'kubernetes-state)
(require 'kubernetes-utils)
(require 'kubernetes-yaml)


;; Components

(defconst kubernetes-persistentvolumeclaims--column-heading
  ["%-24s %10s %10s %15s %6s" "Name Phase Capacity Class Age"])


(kubernetes-ast-define-component persistentvolumeclaim-detail (persistentvolumeclaim)
  (-let [(&alist 'metadata (&alist 'namespace ns 'creationTimestamp time)) persistentvolumeclaim]
    `((section (namespace nil)
               (nav-prop (:namespace-name ,ns)
                         (key-value 12 "Namespace" ,(propertize ns 'face 'kubernetes-namespace))))
      (key-value 12 "Created" ,time))))

(kubernetes-ast-define-component persistentvolumeclaim-line (state persistentvolumeclaim)
  (-let* ((current-time (kubernetes-state--get state 'current-time))
          (pending-deletion (kubernetes-state--get state 'persistentvolumeclaims-pending-deletion))
          (marked-persistentvolumeclaims (kubernetes-state--get state 'marked-persistentvolumeclaims))
          ((&alist 'spec (&alist 'storageClassName storage-class)
                   'status (&alist 'phase phase 'capacity (&alist 'storage capacity))
                   'metadata (&alist 'name name 'creationTimestamp created-time))
           persistentvolumeclaim)
          ([fmt] kubernetes-persistentvolumeclaims--column-heading)
          (list-fmt (split-string fmt))
          (line `(line ,(concat
                         ;; Name
                         (format (pop list-fmt) (s-truncate 21 name))
                         " "
                         ;; Phase
                         (propertize (format (pop list-fmt) phase) 'face 'kubernetes-dimmed)
                         " "
                         ;; Capacity
                         (propertize (format (pop list-fmt) capacity) 'face 'kubernetes-dimmed)
                         " "
                         ;; Storage Class
                         (propertize (format (pop list-fmt) (s-truncate 12 storage-class)) 'face 'kubernetes-dimmed)
                         " "
                         ;; Age
                         (let ((start (apply #'encode-time (kubernetes-utils-parse-utc-timestamp created-time))))
                           (propertize (format (pop list-fmt) (kubernetes--time-diff-string start current-time))
                                       'face 'kubernetes-dimmed))))))
    `(nav-prop (:persistentvolumeclaim-name ,name)
               (copy-prop ,name
                          ,(cond
                            ((member name pending-deletion)
                             `(propertize (face kubernetes-pending-deletion) ,line))
                            ((member name marked-persistentvolumeclaims)
                             `(mark-for-delete ,line))
                            (t
                             line))))))

(kubernetes-ast-define-component persistentvolumeclaim (state persistentvolumeclaim)
  `(section (,(intern (kubernetes-state-resource-name persistentvolumeclaim)) t)
            (heading (persistentvolumeclaim-line ,state ,persistentvolumeclaim))
            (section (details nil)
                     (indent
                      (persistentvolumeclaim-detail ,persistentvolumeclaim)
                      (padding)))))

(kubernetes-ast-define-component persistentvolumeclaims-list (state &optional hidden)
  (-let (((&alist 'items persistentvolumeclaims) (kubernetes-state--get state 'persistentvolumeclaims))
         ([fmt labels] kubernetes-persistentvolumeclaims--column-heading))
    `(section (persistentvolumeclaims-container ,hidden)
              (header-with-count "Persistent Volume Claims" ,persistentvolumeclaims)
              (indent
               (columnar-loading-container ,persistentvolumeclaims
                                           ,(propertize
                                             (apply #'format fmt (split-string labels))
                                             'face
                                             'magit-section-heading)
                                           ,(--map `(persistentvolumeclaim ,state ,it) persistentvolumeclaims)))
              (padding))))


;; Requests and state management

(kubernetes-state-define-refreshers persistentvolumeclaims)

(defun kubernetes-persistentvolumeclaims-delete-marked (state)
  (let ((names (kubernetes-state--get state 'marked-persistentvolumeclaims)))
    (dolist (name names)
      (kubernetes-state-delete-persistentvolumeclaim name)
      (kubernetes-kubectl-delete "persistentvolumeclaim" name state
                                 (lambda (_)
                                   (message "Deleting PVC %s succeeded." name))
                                 (lambda (_)
                                   (message "Deleting PVC %s failed" name)
                                   (kubernetes-state-mark-persistentvolumeclaim name))))
    (kubernetes-state-trigger-redraw)))


;; Displaying PVCs.

(defun kubernetes-persistentvolumeclaims--read-name (state)
  "Read a PVC name from the user.

STATE is the current application state.

Update the PVC state if it not set yet."
  (-let* (((&alist 'items persistentvolumeclaims)
           (or (kubernetes-state--get state 'persistentvolumeclaims)
               (progn
                 (message "Getting persistent volume claims...")
                 (let ((response (kubernetes-kubectl-await-on-async state (-partial #'kubernetes-kubectl-get "persistentvolumeclaims"))))
                   (kubernetes-state-update-persistentvolumeclaims response)
                   response))))
          (persistentvolumeclaims (append persistentvolumeclaims nil))
          (names (-map #'kubernetes-state-resource-name persistentvolumeclaims)))
    (completing-read "PVC: " names nil t)))


(defun kubernetes-display-persistentvolumeclaim (persistentvolumeclaim-name state)
  "Display information about PVC in a new window.

STATE is the current application state.

PVC-NAME is the name of the PVC to display."
  (interactive (let ((state (kubernetes-state)))
                 (list (kubernetes-persistentvolumeclaims--read-name state) state)))
  (if-let (persistentvolumeclaim (kubernetes-state-lookup-persistentvolumeclaim persistentvolumeclaim-name state))
      (select-window
       (display-buffer
        (kubernetes-yaml-make-buffer kubernetes-display-persistentvolumeclaim-buffer-name persistentvolumeclaim)))
    (error "Unknown PVC: %s" persistentvolumeclaim-name)))

(provide 'kubernetes-persistentvolumeclaims)
