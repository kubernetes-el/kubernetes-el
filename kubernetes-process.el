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

(defmethod get-process-for-resource ((ledger kubernetes--process-ledger) resource)
  "Get polling process for RESOURCE in LEDGER."
  (map-elt (slot-value ledger 'poll-processes) resource))

(defmethod poll-process-live-p ((ledger kubernetes--process-ledger) resource)
  "Determine if the polling process for RESOURCE in LEDGER is
live or not."
  (process-live-p (get-process-for-resource ledger resource)))

(defmethod release-all ((ledger kubernetes--process-ledger))
  "Release all processes in LEDGER.

Returns the resources for which processes were released."
  (-flatten
   (map-apply (lambda (key value)
                (release-process-for-resource ledger key)
                (when value key))
              (slot-value ledger 'poll-processes))))

(defmethod release-process-for-resource ((ledger kubernetes--process-ledger)
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

(defmethod set-process-for-resource ((ledger kubernetes--process-ledger)
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

;;; Background polling processes.

(defmacro kubernetes-process--define-polling-process (resource)
  "Create resource polling-related definitions.

RESOURCE is the name of the resource as a symbol.

Defines the following functions:

- `kubernetes-process-set-poll-RESOURCE-process'
- `kubernetes-process-release-poll-RESOURCE-process'
- `kubernetes-process-poll-RESOURCE-process'."
  (unless (symbolp resource) (error "RESOURCE must be a symbol"))
  (let ((proc-var-name (intern (format "kubernetes--internal-poll-%s-process" resource)))
        (proc-live-p (intern (format "kubernetes-process-poll-%s-process-live-p" resource)))
        (releaser-name (intern (format "kubernetes-process-release-poll-%s-process" resource)))
        (setter-name (intern (format "kubernetes-process-set-poll-%s-process" resource))))
    `(progn
       (defvar ,proc-var-name nil
         "Variable used to coordinate polling access to resources.

Do not use this variable directly. Instead, use its corresponding accessors.")

       (defun ,proc-live-p ()
         (poll-process-live-p kubernetes--global-process-ledger (quote ,resource)))

       (defun ,setter-name (proc)
         "Set the polling process to PROC."
         (set-process-for-resource kubernetes--global-process-ledger
                                   (quote ,resource)
                                   proc
                                   t)
         (setq ,proc-var-name proc))

       (defun ,releaser-name ()
         "Kill the existing polling process, if any."
         (release-process-for-resource kubernetes--global-process-ledger (quote ,resource))
         (setq ,proc-var-name nil)))))

(kubernetes-process--define-polling-process config)
(kubernetes-process--define-polling-process configmaps)
(kubernetes-process--define-polling-process deployments)
(kubernetes-process--define-polling-process statefulsets)
(kubernetes-process--define-polling-process ingress)
(kubernetes-process--define-polling-process jobs)
(kubernetes-process--define-polling-process namespaces)
(kubernetes-process--define-polling-process pods)
(kubernetes-process--define-polling-process secrets)
(kubernetes-process--define-polling-process services)
(kubernetes-process--define-polling-process nodes)
(kubernetes-process--define-polling-process persistentvolumeclaims)

(defun kubernetes-process-kill-polling-processes ()
  (release-all kubernetes--global-process-ledger))


(provide 'kubernetes-process)

;;; kubernetes-process.el ends here
