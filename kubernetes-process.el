;;; kubernetes-process.el --- Process management.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 'map)
(require 'request)
(require 'subr-x)

(defun kubernetes--request-option (url &rest body)
  "Send request to URL using BODY, returning error or the response.

This function injects :sync t into BODY."
  (-if-let* ((updated-plist
              (plist-put (plist-put body :sync t)
                         ;; Suppress the default request.el error handler; we
                         ;; check the error later
                         :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                                               nil))))
             (resp (apply #'request url updated-plist))
             (err (request-response-error-thrown resp)))
      (signal 'error (list (cdr err)))
    resp))

(defclass kubernetes--ported-process-record ()
  ((process
    :initarg :process
    :initform nil
    :documentation "Process object.")
   (address
    :initarg :address
    :initform nil
    :documentation "Address corresponding to the process.")
   (port
    :initarg :port
    :initform nil
    :documentation "Port corresponding to the process."))
  "Record for a process with a corresponding network port.")

(cl-defmethod base-url ((record kubernetes--ported-process-record))
  "Get formatted URL for RECORD."
  (format "http://%s:%s" (oref record address) (oref record port)))

(cl-defmethod wait-on-endpoint ((record kubernetes--ported-process-record)
                                endpoint
                                &optional retry-count retry-wait)
  "Wait for RECORD process to respond without error at PORT and ENDPOINT.

Retries RETRY-COUNT times, waiting RETRY-WAIT seconds in between
  each attempt.  Returns the response code.  If ENDPOINT fails to
  respond after retries, signals error."
  (let ((_retry-count (or retry-count 10))
        (_retry-wait (or retry-wait 3))
        (retval))
    (while (and (not retval) (> _retry-count 0))
      (condition-case nil
          (setq retval
                (kubernetes--request-option (format "http://%s:%s/%s"
                                                    (oref record address)
                                                    (oref record port)
                                                    endpoint)))
        (error (progn
                 (setq _retry-count (- _retry-count 1))
                 (sleep-for _retry-wait)))))
    (cond
     ((not retval) (error "Process with port %s was not ready within accepted timeframe"
                          (oref record port)))
     (t (request-response-status-code retval)))))

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
