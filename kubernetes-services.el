;;; kubernetes-services.el --- <enter description here>  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'dash)
(require 'kubernetes-state)
(require 'kubernetes-utils)


(defun kubernetes-services--format-details (service)
  (-let ((detail
          (lambda (key value)
            (when value
              `(copy-prop ,value (key-value 15 ,key ,value)))))

         (format-ports
          (-lambda ((&alist 'name name 'port port 'protocol prot))
            (concat (when name (format "%s:" name))
                    (number-to-string port) "/" prot)))

         ((&alist 'metadata (&alist 'namespace ns
                                    'creationTimestamp created-time)
                  'spec (&alist 'clusterIP internal-ip
                                'externalIPs ips
                                'ports ports))
          service))
    (-non-nil (list (funcall detail "Namespace" ns)
                    (funcall detail "Created" created-time)
                    (funcall detail "Internal IP" internal-ip)
                    (when-let (ips (append ips nil))
                      (funcall detail "External IPs" (string-join ips ", ")))
                    (when-let (ports (append ports nil))
                      (funcall detail "Ports" (string-join (-map format-ports ports) ", ")))))))

(defun kubernetes-services--format-line (service current-time)
  (-let* (((&alist 'metadata (&alist 'name name 'creationTimestamp created-time)
                   'spec (&alist 'clusterIP internal-ip
                                 'externalIPs external-ips))
           service)
          (line `(line ,(concat
                         ;; Name
                         (format "%-30s " (kubernetes--ellipsize name 30))

                         ;; Internal IP
                         (propertize (format "%15s " internal-ip) 'face 'magit-dimmed)

                         ;; External IP
                         (let ((ips (append external-ips nil)))
                           (propertize (format "%15s " (or (car ips) "")) 'face 'magit-dimmed))

                         ;; Age
                         (let ((start (apply #'encode-time (kubernetes--parse-utc-timestamp created-time))))
                           (propertize (format "%6s" (kubernetes--time-diff-string start current-time))
                                       'face 'magit-dimmed))))))
    `(nav-prop (:service-name ,name)
               (copy-prop ,name
                          ,(cond
                            ((member name kubernetes-state--services-pending-deletion)
                             `(propertize (face kubernetes-pending-deletion) ,line))
                            ((member name kubernetes-state--marked-service-names)
                             `(mark-for-delete ,line))
                            (t
                             line))))))

(defun kubernetes-services-render (state &optional hidden)
  (-let* (((&alist 'current-time current-time
                   'services (services-response &as &alist 'items services)) state)
          (services (append services nil))
          (column-heading (propertize (format "%-30s %15s %15s %6s" "Name" "Internal IP" "External IP" "Age") 'face 'magit-section-heading)))
    `(section (services-container ,hidden)
              ,(cond

                ;; If the state is set and there are no services, write "None".
                ((and services-response (null services))
                 `((heading ,(concat (propertize "Services" 'face 'magit-header-line) " (0)"))
                   (indent
                    (section (services-list nil)
                             (propertize (face magit-dimmed) (line "None."))))))

                ;; If there are services, write sections for each service.
                (services
                 (let ((make-entry
                        (lambda (it)
                          `(section (,(intern (kubernetes-state-resource-name it)) t)
                                    (heading ,(kubernetes-services--format-line it current-time))
                                    (indent
                                     (section (details nil)
                                              ,@(kubernetes-services--format-details it)
                                              (padding)))))))
                   `((heading ,(concat (propertize "Services" 'face 'magit-header-line) " " (format "(%s)" (length services))))
                     (indent
                      (line ,column-heading)
                      ,@(-map make-entry services)))))

                ;; If there's no state, assume requests are in progress.
                (t
                 `((heading "Services")
                   (indent
                    (line ,column-heading)
                    (section (services-list nil)
                             (propertize (face kubernetes-progress-indicator) (line "Fetching...")))))))
              (padding))))


(provide 'kubernetes-services)

;;; kubernetes-services.el ends here
