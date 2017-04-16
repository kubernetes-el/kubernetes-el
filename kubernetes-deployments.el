;;; kubernetes-deployments.el --- Rendering for Kubernetes deployments.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)

(require 'kubernetes-kubectl)
(require 'kubernetes-modes)
(require 'kubernetes-props)
(require 'kubernetes-state)
(require 'kubernetes-utils)
(require 'kubernetes-yaml)


;; Requests and state management

(defun kubernetes-deployments-refresh (&optional interactive)
  (unless (kubernetes-process-poll-deployments-process-live-p)
    (kubernetes-process-set-poll-deployments-process
     (kubernetes-kubectl-get-deployments kubernetes-props
                                         (kubernetes-state)
                                         (lambda (response)
                                           (kubernetes-state-update-deployments response)
                                           (when interactive
                                             (message "Updated deployments.")))
                                         (lambda ()
                                           (kubernetes-process-release-poll-deployments-process))))))

(defun kubernetes-deployments-delete-marked (state)
  (let ((names (kubernetes-state-marked-deployments state)))
    (dolist (name names)
      (kubernetes-state-delete-deployment name)
      (kubernetes-kubectl-delete-deployment kubernetes-props state name
                                         (lambda (_)
                                           (message "Deleting deployment %s succeeded." name))
                                         (lambda (_)
                                           (message "Deleting deployment %s failed" name)
                                           (kubernetes-state-mark-deployment name))))
    (kubernetes-state-trigger-redraw)))


;; Displaying deployments

(defun kubernetes-deployments--read-name (state)
  "Read a deployment name from the user.

STATE is the current application state.

Update the deployment state if it not set yet."
  (-let* (((&alist 'items deployments)
           (or (kubernetes-state-deployments state)
               (progn
                 (message "Getting deployments...")
                 (let ((response (kubernetes-kubectl-await-on-async kubernetes-props state #'kubernetes-kubectl-get-deployments)))
                   (kubernetes-state-update-deployments response)
                   response))))
          (deployments (append deployments nil))
          (names (-map #'kubernetes-state-resource-name deployments)))
    (completing-read "Deployment: " names nil t)))

;;;###autoload
(defun kubernetes-display-deployment (deployment-name state)
  "Display information for a deployment in a new window.

STATE is the current application state.

DEPLOYMENT-NAME is the name of the deployment to display."
  (interactive (let ((state (kubernetes-state)))
                 (list (kubernetes-deployments--read-name state) state)))
  (if-let (deployment (kubernetes-state-lookup-deployment deployment-name state))
      (select-window
       (display-buffer
        (kubernetes-yaml-make-buffer kubernetes-display-deployment-buffer-name deployment)))
    (error "Unknown deployment: %s" deployment-name)))


(provide 'kubernetes-deployments)

;;; kubernetes-deployments.el ends here
