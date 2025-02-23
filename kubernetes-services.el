;;; kubernetes-services.el --- Rendering for Kubernetes services.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)

(require 'kubernetes-ast)
(require 'kubernetes-core)
(require 'kubernetes-kubectl)
(require 'kubernetes-loading-container)
(require 'kubernetes-modes)
(require 'kubernetes-state)
(require 'kubernetes-utils)
(require 'kubernetes-vars)
(require 'kubernetes-yaml)


;; Components

(defconst kubernetes-services--column-heading
  ["%-45s %15s %15s %6s" "Name|Internal IP|External IP|Age"])

(defconst kubernetes-services--default-columns
  '((Name (width -45))
    (Internal-IP (width 15))
    (External-IP (width 15))
    (Age (width 6)))
  "Possible columns to select for resource-type services")

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
  ;; (when (not (alist-get 'services-columns state))
  ;;   (setf (alist-get 'services-columns state) kubernetes-services--default-columns))
  (-let* ((current-time (kubernetes-state-current-time state))
          (pending-deletion (kubernetes-state--get state 'services-pending-deletion))
          (marked-services (kubernetes-state--get state 'marked-services))
          ((&alist 'metadata (&alist 'name name 'creationTimestamp created-time)
                   'spec (&alist 'clusterIP internal-ip
                                 'externalIPs external-ips))
           service)
          (line
           (-let* ((row "")
                   ((&alist 'services-columns services-columns) state))
             ;; Read the formatting for the table from the kubernetes-pods--default-columns variable
             (dotimes (i (length services-columns))
               ;; Read the column-width (and create format-string) and header for the current column
               (let* ((col (nth i services-columns))
                      (col-name (car col))
                      (props (cdr col))
                      (width (car (alist-get 'width props)))
                      (fmt (concat "%" (number-to-string width) "s")))
                 ;; Depending on the value of the header we use a specific print function.
                 (setq row (concat row (pcase  col-name
                                         ('Name
                                          (format fmt (s-truncate (abs width) name))
                                          )
                                         ('Internal-IP
                                          (propertize (format fmt internal-ip) 'face 'kubernetes-dimmed))
                                         ('External-IP
                                          (let ((ips (append external-ips nil)))
                                            (propertize (format fmt (or (car ips) "")) 'face 'kubernetes-dimmed)))
                                         ('Age
                                          (let ((start (apply #'encode-time (kubernetes-utils-parse-utc-timestamp created-time))))
                                            (propertize (format fmt (kubernetes--time-diff-string start current-time))
                                                        'face 'kubernetes-dimmed)))
                                         (_
                                          (format "%s " (format fmt "?"))
                                          ))
                                   (unless (= i (1- (length services-columns))) " ")))))
               row)))
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
  (-let* (((&alist 'services-columns column-settings) state)
         ((services-response &as &alist 'items services) (kubernetes-state--get state 'services))
         ([fmt labels] (kubernetes-utils--create-table-headers column-settings)))
    `(section (services-container ,hidden)
              (header-with-count "Services" ,services)
              (indent
               (columnar-loading-container ,services
                                           ,(propertize
                                             (apply #'format fmt (split-string labels "|"))
                                             'face
                                             'magit-section-heading)
                                           ,(--map `(service ,state ,it) services)))
              (padding))))

;; Requests and state management

(kubernetes-state-define-refreshers services)

(defun kubernetes-services-delete-marked (state)
  (let ((names (kubernetes-state--get state 'marked-services)))
    (dolist (name names)
      (kubernetes-state-delete-service name)
      (kubernetes-kubectl-delete "service" name state
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
           (or (kubernetes-state--get state 'services)
               (progn
                 (message "Getting services...")
                 (let ((response (kubernetes-kubectl-await-on-async state (-partial #'kubernetes-kubectl-get "services"))))
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
