;;; kubernetes-ingress.el --- Rendering routines for Kubernetes ingress.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)

(require 'kubernetes-ast)
(require 'kubernetes-loading-container)
(require 'kubernetes-modes)
(require 'kubernetes-props)
(require 'kubernetes-state)
(require 'kubernetes-utils)
(require 'kubernetes-yaml)


;; Components

(defconst kubernetes-ingress--column-heading
  (propertize (format "%-45s %-25s %20s %10s" "Name" "Hosts" "Address"  "Age")
              'face 'magit-section-heading))

(kubernetes-ast-define-component ingress-detail (ingress)
  (-let [(&alist 'metadata (&alist 'namespace ns 'creationTimestamp time)) ingress]
    `((section (namespace nil)
               (nav-prop (:namespace-name ,ns)
                         (key-value 12 "Namespace" ,(propertize ns 'face 'kubernetes-namespace))))
      (key-value 12 "Created" ,time))))

(kubernetes-ast-define-component ingress-line (state ingress)
  (-let* ((current-time (kubernetes-state-current-time state))
          (pending-deletion (kubernetes-state-ingress-pending-deletion state))
          (marked-ingress (kubernetes-state-marked-ingress state))
          ((&alist 'metadata (&alist 'name name 'creationTimestamp created-time)
                   'spec (&alist 'rules ingress-rules)
                   'status (&alist 'loadBalancer (&alist 'ingress ingress-lb-list)))
           ingress)
          (line `(line ,(concat
                         ;; Name
                         (format "%-45s " (kubernetes-utils-ellipsize name 45))

                         ;; Hosts
                         (format "%-25s " (--mapcat (alist-get 'host it) ingress-rules))

                         ;; Address
                          (format "%20s "
                                  (mapconcat
                                   'identity
                                   (mapcar
                                    (lambda (i) (format "%s" (alist-get 'ip   i )))
                                    ingress-lb-list)
                                   ", "))

                         ;; Age
                         (let ((start (apply #'encode-time (kubernetes-utils-parse-utc-timestamp created-time))))
                           (propertize (format "%10s" (kubernetes-utils-time-diff-string start current-time))
                                       'face 'magit-dimmed))))))

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
  (-let [(&alist 'items ingress) (kubernetes-state-ingress state)]
    `(section (ingress-container ,hidden)
              (header-with-count "Ingress" ,ingress)
              (indent
               (columnar-loading-container ,ingress ,kubernetes-ingress--column-heading
                                           ,(--map `(ingress ,state ,it) ingress)))
              (padding))))

;; Requests and state management

(kubernetes-state-define-refreshers ingress)

(defun kubernetes-ingress-delete-marked (state)
  (let ((names (kubernetes-state-marked-ingress state)))
    (dolist (name names)
      (kubernetes-state-delete-ingress name)
      (kubernetes-kubectl-delete-ingress kubernetes-props state name
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
           (or (kubernetes-state-ingress state)
               (progn
                 (message "Getting ingress...")
                 (let ((response (kubernetes-kubectl-await-on-async kubernetes-props state #'kubernetes-kubectl-get-ingress)))
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
