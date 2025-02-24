;;; kubernetes-networkpolicies.el --- Rendering for Kubernetes network policies  -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:

(require 'dash)
(require 's)

(require 'kubernetes-ast)
(require 'kubernetes-commands)
(require 'kubernetes-core)
(require 'kubernetes-loading-container)
(require 'kubernetes-state)
(require 'kubernetes-utils)
(require 'kubernetes-vars)
(require 'kubernetes-yaml)


;; Column Heading
(defconst kubernetes-networkpolicies--column-heading
  ["%-36s %-15s %7s %6s" "Name Namespace Ingress Egress"])

;; Component Definitions
(kubernetes-ast-define-component networkpolicy-detail (networkpolicy)
  (-let [(&alist 'metadata (&alist 'namespace ns 'creationTimestamp time 'name name)
                 'spec (&alist 'ingress ingress 'egress egress))
         networkpolicy]
    `((section (namespace nil)
               (nav-prop (:namespace-name ,ns)
                         (key-value 12 "Namespace" ,(propertize ns 'face 'kubernetes-namespace))))
      (key-value 12 "Name" ,name)
      (key-value 12 "Created" ,time))))


(kubernetes-ast-define-component networkpolicy-line (state networkpolicy)
  (-let* ((current-time (kubernetes-state--get state 'current-time))
          (pending-deletion (kubernetes-state--get state 'networkpolicies-pending-deletion))
          (marked-networkpolicies (kubernetes-state--get state 'marked-networkpolicies))
          ((&alist 'metadata (&alist 'name name 'namespace namespace)
                   'spec (&alist 'policyTypes policyTypes 'ingress ingress 'egress egress))
           networkpolicy)
          (ingress-descr (if (seq-contains-p policyTypes "Ingress")
                             "yes" "no"))
          (egress-descr (if (seq-contains-p policyTypes "Egress") "yes" "no"))
          ([fmt] kubernetes-networkpolicies--column-heading)
          (list-fmt  (split-string fmt))
          (line `(line ,(concat
                         ;; Name
                         (propertize (format (pop list-fmt) (s-truncate 33 name)) 'face 'default)
                         " "
                         ;; Namespace
                         (propertize (format (pop list-fmt) namespace) 'face 'kubernetes-dimmed)
                         " "
                         (propertize (format (pop list-fmt) ingress-descr) 'face 'kubernetes-dimmed)
                         " "
                         (propertize (format (pop list-fmt) egress-descr) 'face 'kubernetes-dimmed)
                         ))))
    `(nav-prop (:networkpolicy-name ,name)
               (copy-prop ,name
                          ,(cond
                            ((member name pending-deletion)
                             `(propertize (face kubernetes-pending-deletion) ,line))
                            ((member name marked-networkpolicies)
                             `(mark-for-delete ,line))
                            (t
                             line))))))


(kubernetes-ast-define-component networkpolicy (state networkpolicy)
  `(section (,(intern (kubernetes-state-resource-name networkpolicy)) t)
            (heading (networkpolicy-line ,state ,networkpolicy))
            (indent
             (networkpolicy-detail ,networkpolicy)
             (padding))))

(kubernetes-ast-define-component networkpolicies-list (state &optional hidden)
  (-let (((&alist 'items networkpolicies) (kubernetes-state--get state 'networkpolicies))
         ([fmt labels] kubernetes-networkpolicies--column-heading))
    `(section (networkpolicies-container ,hidden)
              (header-with-count "Network Policies" ,networkpolicies)

              (indent
               (columnar-loading-container ,networkpolicies
                                           ,(propertize
                                             (apply #'format fmt (split-string labels))
                                             'face
                                             'magit-section-heading)
                                           ,(--map `(networkpolicy ,state ,it) networkpolicies)))
              (padding))))

;; Requests and State Management

(kubernetes-state-define-refreshers networkpolicies)

(defun kubernetes-networkpolicies-delete-marked (state)
  "Delete marked networkpolicies.

STATE is the current application state."
  (let ((names (kubernetes-state--get state 'marked-networkpolicies)))
    (dolist (name names)
      (kubernetes-state-delete-networkpolicy name)
      (kubernetes-kubectl-delete "networkpolicy" name state
                                 (lambda (_)
                                   (message "Deleting Network Policy %s succeeded." name))
                                 (lambda (_)
                                   (message "Deleting Network Policy %s failed" name)
                                   (kubernetes-state-mark-networkpolicy name))))
    (kubernetes-state-trigger-redraw)))

;; Displaying Network Policies

(defun kubernetes-networkpolicies--read-name (state)
  "Read a network policy name from the user.
STATE is the current application state."
  (-let* (((&alist 'items networkpolicies)
           (or (kubernetes-state--get state 'networkpolicies)
               (progn
                 (message "Getting network policies...")
                 (let ((response (kubernetes-kubectl-await-on-async state (-partial #'kubernetes-kubectl-get "networkpolicies"))))
                   (kubernetes-state-update-networkpolicies response)
                   response))))
          (names (-map #'kubernetes-state-resource-name networkpolicies)))
    (completing-read "Network Policy: " names nil t)))

(defun kubernetes-display-networkpolicy (networkpolicy-name state)
  "Display information for a network policy in a new window.
STATE is the current application state.
NETWORKPOLICY-NAME is the name of the network policy to display."
  (interactive (let ((state (kubernetes-state)))
                 (list (kubernetes-networkpolicies--read-name state) state)))
  (if-let (networkpolicy (kubernetes-state-lookup-networkpolicy networkpolicy-name state))
      (select-window
       (display-buffer
        (kubernetes-yaml-make-buffer kubernetes-display-networkpolicy-buffer-name networkpolicy)))
    (error "Unknown Network Policy: %s" networkpolicy-name)))

(provide 'kubernetes-networkpolicies)

;;; kubernetes-networkpolicies.el ends here
