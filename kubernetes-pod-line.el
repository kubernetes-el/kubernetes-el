;;; kubernetes-pod-line.el --- Component for individual pod lines.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'kubernetes-ast)
(require 'kubernetes-state)
(require 'kubernetes-utils)

(defun kubernetes-pod-line-ok-p (pod)
  (-let [(&alist 'status (&alist 'containerStatuses containers 'phase phase)) pod pod]
    (unless (seq-empty-p containers)
      (-let* (([(&alist 'state pod-state)] containers)
              (pod-state (or (alist-get 'reason (alist-get 'waiting pod-state)) phase)))
        (member (downcase pod-state) '("running" "containercreating" "terminated"
                                       "succeeded"))))))

(kubernetes-ast-define-component pod-line (state pod)
  (-let* ((marked-pods (kubernetes-state-marked-pods state))
          (pending-deletion (kubernetes-state-pods-pending-deletion state))
          ((&alist 'metadata (&alist 'name name) 'status (&alist 'containerStatuses containers 'phase phase)) pod)
          ([(&alist 'state pod-state)] containers)
          (pod-state (or (alist-get 'reason (alist-get 'waiting pod-state)) phase))
          (state-face
           (cond
            ((member (downcase pod-state) '("running" "containercreating" "terminated"))
             'magit-dimmed)
            ((member (downcase pod-state) '("runcontainererror" "crashloopbackoff"))
             'error)
            ((equal (downcase pod-state) "succeeded")
             'success)
            (t
             'warning)))
          (line
           (concat (propertize (format "%-11s " (kubernetes-utils-ellipsize pod-state 11)) 'face state-face)
                   name)))

    `(section (,(intern (kubernetes-state-resource-name pod)) t)
              (nav-prop (:pod-name ,name)
                        (copy-prop ,name
                                   (line ,(cond
                                           ((member name pending-deletion)
                                            `(propertize (face kubernetes-pending-deletion) ,line))
                                           ((member name marked-pods)
                                            `(mark-for-delete ,line))
                                           (t
                                            line))))))))


(provide 'kubernetes-pod-line)

;;; kubernetes-pod-line.el ends here
