;;; kubernetes-services.el --- Rendering for Kubernetes services.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)

(require 'kubernetes-ast)
(require 'kubernetes-kubectl)
(require 'kubernetes-loading-container)
(require 'kubernetes-modes)
(require 'kubernetes-props)
(require 'kubernetes-state)
(require 'kubernetes-utils)
(require 'kubernetes-vars)
(require 'kubernetes-yaml)


;; Components

(defconst kubernetes-services--column-heading
  (propertize (format "%-30s %15s %15s %6s" "Name" "Internal IP" "External IP" "Age")
              'face 'magit-section-heading))

(kubernetes-ast-define-component service-details (service)
  (-let ((detail
          (lambda (key value)
            (when value
              `(key-value 15 ,key ,value))))

         (format-ports
          (-lambda ((&alist 'name name 'port port 'protocol prot))
            (concat (when name (format "%s:" name))
                    (number-to-string port) "/" prot)))

         ((&alist 'metadata (&alist 'namespace ns
                                    'creationTimestamp created-time
                                    'labels (&alist 'name label))
                  'spec (&alist 'clusterIP internal-ip
                                'externalIPs ips
                                'ports ports
                                'selector (&alist 'name selector)))
          service))
    (list
     (when selector
       `(section (selector nil)
                 (nav-prop (:selector ,selector) ,(funcall detail "Selector" (propertize selector 'face 'kubernetes-selector)))))
     (funcall detail "Label" label)
     (when ns
       `(section (namespace nil)
                 (nav-prop (:namespace-name ,ns)
                           ,(funcall detail "Namespace" (propertize ns 'face 'kubernetes-namespace)))))
     (funcall detail "Created" created-time)
     (funcall detail "Internal IP" internal-ip)
     (when-let (ips (append ips nil))
       (funcall detail "External IPs" (string-join ips ", ")))
     (funcall detail "Ports" (string-join (seq-map format-ports ports) ", ")))))

(kubernetes-ast-define-component service-line (state service)
  (-let* ((current-time (kubernetes-state-current-time state))
          (pending-deletion (kubernetes-state-services-pending-deletion state))
          (marked-services (kubernetes-state-marked-services state))
          ((&alist 'metadata (&alist 'name name 'creationTimestamp created-time)
                   'spec (&alist 'clusterIP internal-ip
                                 'externalIPs external-ips))
           service)
          (line `(line ,(concat
                         ;; Name
                         (format "%-30s " (kubernetes-utils-ellipsize name 30))

                         ;; Internal IP
                         (propertize (format "%15s " internal-ip) 'face 'magit-dimmed)

                         ;; External IP
                         (let ((ips (append external-ips nil)))
                           (propertize (format "%15s " (or (car ips) "")) 'face 'magit-dimmed))

                         ;; Age
                         (let ((start (apply #'encode-time (kubernetes-utils-parse-utc-timestamp created-time))))
                           (propertize (format "%6s" (kubernetes-utils-time-diff-string start current-time))
                                       'face 'magit-dimmed))))))
    `(nav-prop (:service-name ,name)
               (copy-prop ,name
                          ,(cond
                            ((member name pending-deletion)
                             `(propertize (face kubernetes-pending-deletion) ,line))
                            ((member name marked-services)
                             `(mark-for-delete ,line))
                            (t
                             line))))))

(kubernetes-ast-define-component service (state service)
  `(section (,(intern (kubernetes-state-resource-name service)) t)
            (heading (service-line ,state ,service))
            (indent
             (section (details nil)
                      (service-details ,service)
                      (padding)))))

(kubernetes-ast-define-component services-list (state &optional hidden)
  (-let [(services-response &as &alist 'items services) (kubernetes-state-services state)]
    `(section (services-container ,hidden)
              (header-with-count "Services" ,services)
              (indent
               (columnar-loading-container ,services ,kubernetes-services--column-heading
                                           ,(--map `(service ,state ,it) services)))
              (padding))))


;; Requests and state management

(defun kubernetes-services-refresh (&optional interactive)
  (unless (kubernetes-process-poll-services-process-live-p)
    (kubernetes-process-set-poll-services-process
     (kubernetes-kubectl-get-services kubernetes-props
                                      (kubernetes-state)
                                      (lambda (response)
                                        (kubernetes-state-update-services response)
                                        (when interactive
                                          (message "Updated services.")))
                                      (lambda ()
                                        (kubernetes-process-release-poll-services-process))))))

(defun kubernetes-services-delete-marked (state)
  (let ((names (kubernetes-state-marked-services state)))
    (dolist (name names)
      (kubernetes-state-delete-service name)
      (kubernetes-kubectl-delete-service kubernetes-props state name
                                         (lambda (_)
                                           (message "Deleting service %s succeeded." name))
                                         (lambda (_)
                                           (message "Deleting service %s failed" name)
                                           (kubernetes-state-mark-service name))))
    (kubernetes-state-trigger-redraw)))


;; Displaying services

(defun kubernetes-services--read-name (state)
  "Read a service name from the user.

STATE is the current application state.

Update the service state if it not set yet."
  (-let* (((&alist 'items services)
           (or (kubernetes-state-services state)
               (progn
                 (message "Getting services...")
                 (let ((response (kubernetes-kubectl-await-on-async kubernetes-props state #'kubernetes-kubectl-get-services)))
                   (kubernetes-state-update-services response)
                   response))))
          (services (append services nil))
          (names (-map #'kubernetes-state-resource-name services)))
    (completing-read "Service: " names nil t)))

;;;###autoload
(defun kubernetes-display-service (service-name state)
  "Display information for a service in a new window.

STATE is the current application state.

SERVICE-NAME is the name of the service to display."
  (interactive (let ((state (kubernetes-state)))
                 (list (kubernetes-services--read-name state) state)))
  (if-let (service (kubernetes-state-lookup-service service-name state))
      (select-window
       (display-buffer
        (kubernetes-yaml-make-buffer kubernetes-display-service-buffer-name service)))
    (error "Unknown service: %s" service-name)))


(provide 'kubernetes-services)

;;; kubernetes-services.el ends here
