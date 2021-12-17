;;; kubernetes-process.el --- Process management.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 'map)
(require 'subr-x)

(defclass kubernetes--process-ledger ()
  ((poll-processes
    :initarg :poll-processes
    :initform '()
    :documentation "Mapping between resources and their polling
  processes. Keys are expected to be in plural form, i.e. 'pods'
  instead of 'pod' and so on."))
  "Track Kubernetes processes.")

(cl-defmethod get-process-for-resource ((ledger kubernetes--process-ledger) resource)
  "Get polling process for RESOURCE in LEDGER."
  (map-elt (slot-value ledger 'poll-processes) resource))

(cl-defmethod poll-process-live-p ((ledger kubernetes--process-ledger) resource)
  "Determine if the polling process for RESOURCE in LEDGER is
live or not."
  (process-live-p (get-process-for-resource ledger resource)))

(cl-defmethod release-all ((ledger kubernetes--process-ledger))
  "Release all processes in LEDGER.

Returns the resources for which processes were released."
  (-flatten
   (map-apply (lambda (key value)
                (release-process-for-resource ledger key)
                (when value key))
              (slot-value ledger 'poll-processes))))

(cl-defmethod release-process-for-resource ((ledger kubernetes--process-ledger)
                                         resource)
  "Terminate the polling process for RESOURCE in LEDGER and remove it from the ledger.

This function is a no-op if there is no process.

If the process is already dead, clean it up."
  (-when-let* ((proc (get-process-for-resource ledger resource)))
    (when (process-live-p proc)
      (kubernetes-process-kill-quietly proc))
    (if (not (slot-value ledger 'poll-processes))
        (object-add-to-list ledger 'poll-processes '(resource nil))
      (setf (alist-get resource (slot-value ledger 'poll-processes)) nil))))

(cl-defmethod set-process-for-resource ((ledger kubernetes--process-ledger)
                                     resource proc
                                     &optional force)
  "Assigns process PROC as the polling process for RESOURCE in
LEDGER."
  (when (poll-process-live-p ledger resource)
    (if force
        (release-process-for-resource ledger resource)
      (error "Live poll process already present for `%s'; terminate first."
             resource)))
  (if (not (slot-value ledger 'poll-processes))
      (object-add-to-list ledger 'poll-processes `(,resource . ,proc))
    (setf (alist-get resource (slot-value ledger 'poll-processes)) proc)))

(defvar kubernetes--global-process-ledger (kubernetes--process-ledger)
  "Global process tracker for kubernetes-el.")

(defun kubernetes-process-kill-quietly (proc &optional _signal)
  "Kill process PROC silently and the associated buffer, suppressing all errors."
  (when proc
    (set-process-sentinel proc nil)
    (set-process-query-on-exit-flag proc nil)
    (let ((kill-buffer-query-functions nil)
          (buf (process-buffer proc)))
      (ignore-errors (kill-process proc))
      (ignore-errors (delete-process proc))
      (ignore-errors (kill-buffer buf)))))

(defun kubernetes-process-kill-polling-processes ()
  "Kill all in-flight polling processes."
  (release-all kubernetes--global-process-ledger))


(provide 'kubernetes-process)

;;; kubernetes-process.el ends here
