;;; kubernetes-configmaps.el --- Rendering for Kubernetes configmaps.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)

(require 'kubernetes-ast)
(require 'kubernetes-core)
(require 'kubernetes-loading-container)
(require 'kubernetes-modes)
(require 'kubernetes-process)
(require 'kubernetes-state)
(require 'kubernetes-utils)
(require 'kubernetes-yaml)


;; Components

(defconst kubernetes-configmaps--column-heading
  ["%-45s %6s %6s" "Name Data Age"])

(defconst kubernetes-configmaps--default-columns
  '((Name (width -45))
    (Data (width 6))
    (Age (width 6)))
  "Possible columns to select for resource-type configmaps")

(kubernetes-ast-define-component configmap-data (configmap)
  (-let [(&alist 'data data) configmap]
    `(section (data-container t)
      (header-with-count "Data:" ,data)
      (indent
       ,@(mapcar
          (lambda (pair)
            (cl-destructuring-bind (key . val) pair
              `(key-value
                16
                ,(s-truncate 12 (symbol-name key))
                ,(s-truncate 18 val))))
          data)))))

(kubernetes-ast-define-component configmap-detail (configmap)
  (-let [(&alist 'metadata (&alist 'namespace ns 'creationTimestamp time)) configmap]
    `((section (namespace nil)
               (nav-prop (:namespace-name ,ns)
                         (key-value 12 "Namespace" ,(propertize ns 'face 'kubernetes-namespace))))
      (key-value 12 "Created" ,time))))

(kubernetes-ast-define-component configmap-line (state configmap)
  ;; (when (not (alist-get 'configmaps-columns state))
  ;;   (setf (alist-get 'configmaps-columns state) kubernetes-configmaps--default-columns))
  (-let* ((current-time (kubernetes-state--get state 'current-time))
          (pending-deletion (kubernetes-state--get state 'configmaps-pending-deletion))
          (marked-configmaps (kubernetes-state--get state 'marked-configmaps))
          ((&alist 'data data
                   'metadata (&alist 'name name 'creationTimestamp created-time))
           configmap)
          (line
           (-let* ((row "")
                 ((&alist 'configmaps-columns configmaps-columns) state))
             ;; Read the formatting for the table from the kubernetes-pods--default-columns variable
             (dotimes (i (length configmaps-columns))
               ;; Read the column-width (and create format-string) and header for the current column
               (let* ((col (nth i configmaps-columns))
                      (col-name (car col))
                      (props (cdr col))
                      (width (car (alist-get 'width props)))
                      (fmt (concat "%" (number-to-string width) "s")))
                 ;; Depending on the value of the header we use a specific print function.
                 (setq row (concat  row (pcase  col-name
                                          ('Name
                                           (format fmt (s-truncate (abs width) name))
                                           )
                                          ('Data
                                           (propertize (format fmt (seq-length data)) 'face 'kubernetes-dimmed)
                                           )
                                          ('Age
                                           (let ((start (apply #'encode-time (kubernetes-utils-parse-utc-timestamp created-time))))
                                             (propertize (format fmt (kubernetes--time-diff-string start current-time))
                                                         'face 'kubernetes-dimmed))
                                           )
                                          (_
                                           (format "%s " (format fmt "?"))
                                           ))
                                    (unless (= i (1- (length configmaps-columns))) " ")))))
             row)))
    `(nav-prop (:configmap-name ,name)
               (copy-prop ,name
                          ,(cond
                            ((member name pending-deletion)
                             `(propertize (face kubernetes-pending-deletion) ,line))
                            ((member name marked-configmaps)
                             `(mark-for-delete ,line))
                            (t
                             line))))))

(kubernetes-ast-define-component configmap (state configmap)
  `(section (,(intern (kubernetes-state-resource-name configmap)) t)
            (heading (configmap-line ,state ,configmap))
            (section (details nil)
                     (indent
                      (configmap-detail ,configmap)
                      (configmap-data ,configmap)
                      (padding)))))

(kubernetes-ast-define-component configmaps-list (state &optional hidden)
  (-let (((&alist 'items configmaps) (kubernetes-state--get state 'configmaps))
         ([fmt labels] kubernetes-configmaps--column-heading))
    `(section (configmaps-container ,hidden)
              (header-with-count "Configmaps" ,configmaps)
              (indent
               (columnar-loading-container ,configmaps
                                           ,(propertize
                                             (apply #'format fmt (split-string labels))
                                             'face
                                             'magit-section-heading)
                                           ,(--map `(configmap ,state ,it) configmaps)))
              (padding))))


;; Requests and state management

(kubernetes-state-define-refreshers configmaps)

(defun kubernetes-configmaps-delete-marked (state)
  (let ((names (kubernetes-state--get state 'marked-configmaps)))
    (dolist (name names)
      (kubernetes-state-delete-configmap name)
      (kubernetes-kubectl-delete "configmap" name state
                                 (lambda (_)
                                   (message "Deleting configmap %s succeeded." name))
                                 (lambda (_)
                                   (message "Deleting configmap %s failed" name)
                                   (kubernetes-state-mark-configmap name))))
    (kubernetes-state-trigger-redraw)))


;; Displaying configmaps.

(defun kubernetes-configmaps--read-name (state)
  "Read a configmap name from the user.

STATE is the current application state.

Update the configmap state if it not set yet."
  (-let* (((&alist 'items configmaps)
           (or (kubernetes-state--get state 'configmaps)
               (progn
                 (message "Getting configmaps...")
                 (let ((response (kubernetes-kubectl-await-on-async state (-partial #'kubernetes-kubectl-get "configmaps"))))
                   (kubernetes-state-update-configmaps response)
                   response))))
          (configmaps (append configmaps nil))
          (names (-map #'kubernetes-state-resource-name configmaps)))
    (completing-read "Configmap: " names nil t)))


;;;###autoload
(defun kubernetes-display-configmap (configmap-name state)
  "Display information for a configmap in a new window.

STATE is the current application state.

CONFIGMAP-NAME is the name of the configmap to display."
  (interactive (let ((state (kubernetes-state)))
                 (list (kubernetes-configmaps--read-name state) state)))
  (if-let (configmap (kubernetes-state-lookup-configmap configmap-name state))
      (select-window
       (display-buffer
        (kubernetes-yaml-make-buffer kubernetes-display-configmap-buffer-name configmap)))
    (error "Unknown configmap: %s" configmap-name)))


(provide 'kubernetes-configmaps)

;;; kubernetes-configmaps.el ends here
