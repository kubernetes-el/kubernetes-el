;;; kubernetes-pod-line.el --- Component for individual pod lines.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 's)

(require 'kubernetes-ast)
(require 'kubernetes-state)
(require 'kubernetes-utils)

(defun kubernetes-pod-line-ok-p (pod)
  "Determine if POD should be displayed with a warning or not."
  (-let [(&alist 'status (&alist 'containerStatuses containers 'phase phase)) pod]
    (unless (seq-empty-p containers)
      (-let* (([(&alist 'state pod-state)] containers)
              (pod-state (or (alist-get 'reason (alist-get 'waiting pod-state)) phase)))
        (member (downcase pod-state) '("running" "containercreating" "terminated"
                                       "succeeded"))))))

(kubernetes-ast-define-component pod-line (state pod)
  (-let* ((marked-pods (kubernetes-state--get state 'marked-pods))
          (pending-deletion (kubernetes-state--get state 'pods-pending-deletion))
          ((&alist 'metadata (&alist 'name name) 'status (&alist 'containerStatuses containers 'phase phase)) pod)
          ([(&alist 'state pod-state)] containers)
          (pod-state (or (alist-get 'reason (alist-get 'waiting pod-state)) phase))
          (state-face
           (cond
            ((member (downcase pod-state) '("running" "containercreating" "terminated"))
             'kubernetes-dimmed)
            ((member (downcase pod-state) '("runcontainererror" "crashloopbackoff"))
             'error)
            ((equal (downcase pod-state) "succeeded")
             'success)
            (t
             'warning)))
          (line
           (concat (propertize (format "%-11s " (s-truncate 11 pod-state)) 'face state-face)
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
