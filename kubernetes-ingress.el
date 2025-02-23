;;; kubernetes-ingress.el --- Rendering routines for Kubernetes ingress.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)

(require 'kubernetes-ast)
(require 'kubernetes-core)
(require 'kubernetes-loading-container)
(require 'kubernetes-modes)
(require 'kubernetes-state)
(require 'kubernetes-utils)
(require 'kubernetes-yaml)
(require 'magit-section)


;; Components

(defconst kubernetes-ingress--column-heading
  (propertize (format "%-45s %-25s %20s %10s" "Name" "Hosts" "Address"  "Age")
              'face 'magit-section-heading))

(defconst kubernetes-ingress--default-columns
  '((Name (width -45))
    (Hosts (width -25))
    (Address (width 20))
    (Age (width 10)))
  "Possible columns to select for resource-type ingress")

(kubernetes-ast-define-component ingress-detail (ingress)
  (-let [(&alist 'metadata (&alist 'namespace ns 'creationTimestamp time)) ingress]
    `((section (namespace nil)
               (nav-prop (:namespace-name ,ns)
                         (key-value 12 "Namespace" ,(propertize ns 'face 'kubernetes-namespace))))
      (key-value 12 "Created" ,time))))

(kubernetes-ast-define-component ingress-line (state ingress)
  ;; (when (not (alist-get 'ingress-columns state))
  ;;   (setf (alist-get 'ingress-columns state) kubernetes-ingress--default-columns))
  (-let* ((current-time (kubernetes-state-current-time state))
          (pending-deletion (kubernetes-state--get state 'ingress-pending-deletion))
          (marked-ingress (kubernetes-state--get state 'marked-ingress))
          ((&alist 'metadata (&alist 'name name 'creationTimestamp created-time)
                   'spec (&alist 'rules ingress-rules)
                   'status (&alist 'loadBalancer (&alist 'ingress ingress-lb-list)))
           ingress)
          (line
           (-let* ((row "")
                   ((&alist 'ingress-columns ingress-columns) state))
             ;; Read the formatting for the table from the kubernetes-pods--default-columns variable
             (dotimes (i (length ingress-columns))
               ;; Read the column-width (and create format-string) and header for the current column
               (let* ((col (nth i ingress-columns))
                      (col-name (car col))
                      (props (cdr col))
                      (width (car (alist-get 'width props)))
                      (fmt (concat "%" (number-to-string width) "s")))
                 ;; Depending on the value of the header we use a specific print function.
                 (setq row (concat  row (pcase  col-name
                                          ('Name
                                           (format fmt (s-truncate (abs width) name))
                                           )
                                          ('Hosts
                                           (format fmt (s-truncate (abs width) (mapconcat (-partial 'alist-get 'host) ingress-rules ", ")))
                                           )
                                          ('Address
                                           (format fmt (s-truncate (abs width) (mapconcat (-partial 'alist-get 'ip) ingress-lb-list ", ")))
                                           )
                                          ('Age
                                           (let ((start (apply #'encode-time (kubernetes-utils-parse-utc-timestamp created-time))))
                                             (propertize (format fmt (kubernetes--time-diff-string start current-time))
                                                         'face 'kubernetes-dimmed))
                                           )
                                          (_
                                           (format "%s " (format fmt "?"))
                                           ))
                                    (unless (= i (1- (length ingress-columns))) " ")))))
             row)))
    `(nav-prop (:ingress-name ,name)
               (copy-prop ,name
                          ,(cond
                             ((member name pending-deletion)
                              `(propertize (face kubernetes-pending-deletion) ,line))
                             ((member name marked-ingress)
                              `(mark-for-delete ,line))
                            (t
                             line))))))

(kubernetes-ast-define-component ingress (state ingress)
  `(section (,(intern (kubernetes-state-resource-name ingress)) t)
            (heading (ingress-line ,state ,ingress))
            (section (details nil)
                     (indent
                      (ingress-detail ,ingress)
                      (padding)))))

(kubernetes-ast-define-component ingress-list (state &optional hidden)
  (-let* (((&alist 'ingress-columns column-settings) state)
         ((&alist 'items ingress) (kubernetes-state--get state 'ingress))
         ([fmt labels] (kubernetes-utils--create-table-headers column-settings)))
        `(section (ingress-container ,hidden)
                  (header-with-count "Ingress" ,ingress)
                  (indent
                   (columnar-loading-container ,ingress
                                               ,(propertize
                                                 (apply #'format fmt (split-string labels "|"))
                                                 'face
                                                 'magit-section-heading)
                                               ,(--map `(ingress ,state ,it) ingress)))
                  (padding))))

;; Requests and state management

(kubernetes-state-define-refreshers ingress)

(defun kubernetes-ingress-delete-marked (state)
  (let ((names (kubernetes-state--get state 'marked-ingress)))
    (dolist (name names)
      (kubernetes-state-delete-ingress name)
      (kubernetes-kubectl-delete "ingress" name state
                                        (lambda (_)
                                          (message "Deleting ingress %s succeeded." name))
                                        (lambda (_)
                                          (message "Deleting ingress %s failed" name)
                                          (kubernetes-state-mark-ingress name))))
    (kubernetes-state-trigger-redraw)))

;; Displaying ingress

(defun kubernetes-ingress--read-name (state)
  "Read a ingress name from the user.

STATE is the current application state.

Update the ingress state if it not set yet."
  (-let* (((&alist 'items ingress)
           (or (kubernetes-state--get state 'ingress)
               (progn
                 (message "Getting ingress...")
                 (let ((response (kubernetes-kubectl-await-on-async state (-partial #'kubernetes-kubectl-get "ingress"))))
                   (kubernetes-state-update-ingress response)
                   response))))
          (ingress (append ingress nil))
          (names (-map #'kubernetes-state-resource-name ingress)))
    (completing-read "Ingress: " names nil t)))

;;;###autoload
(defun kubernetes-display-ingress (ingress-name state)
  "Display information for a ingress in a new window.

STATE is the current application state.

INGRESS-NAME is the name of the ingress to display."
  (interactive (let ((state (kubernetes-state)))
                 (list (kubernetes-ingress--read-name state) state)))
  (if-let (ingress (kubernetes-state-lookup-ingress ingress-name state))
      (select-window
       (display-buffer
        (kubernetes-yaml-make-buffer kubernetes-display-ingress-buffer-name ingress)))
    (error "Unknown ingress: %s" ingress-name)))



(provide 'kubernetes-ingress)

;;; kubernetes-ingress.el ends here
