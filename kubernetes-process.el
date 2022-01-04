;;; kubernetes-process.el --- Process management.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 'map)
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

(cl-defmethod wait-on-endpoint ((record kubernetes--ported-process-record)
                                endpoint
                                &optional retry-count retry-wait)
  "Wait for RECORD process to respond without error at PORT and ENDPOINT.

Retries RETRY-COUNT times, waiting RETRY-WAIT seconds in between
  each attempt.  Returns the response code.  If ENDPOINT fails to
  respond after retries, signals error."
  (let ((_retry-count (or retry-count 5))
        (_retry-wait (or retry-wait 2))
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
  instead of 'pod' and so on.")
   (proxy
    :initarg :proxy
    :initform nil
    :documentation "Proxy process. There should only be one of these at a time."))
  "Track Kubernetes processes.")

(cl-defmethod proxy-active-p ((ledger kubernetes--process-ledger))
  "Determine if the proxy process in LEDGER is active or not."
  (-if-let* ((proxy-record  (oref ledger proxy))
             (proxy-proc (oref proxy-record :process)))
      (process-live-p proxy-proc)))

(cl-defmethod proxy-ready-p ((ledger kubernetes--process-ledger))
  "Wait for proxy process of LEDGER to be ready for requests.

Returns nil if the proxy has started, but either readyz or livez
  endpoints return non-200."
  (let ((proxy-record (oref ledger proxy)))
    (and (= 200 (wait-on-endpoint proxy-record "readyz"))
         (= 200 (wait-on-endpoint proxy-record "livez")))))

(defun kubernetes--val-from-arg-list (arg-list key)
  (when arg-list
    (when-let ((key-index (-elem-index (format "--%s" (symbol-name key)) arg-list)))
      (nth (+ 1 key-index) arg-list))))

(setq jjin/proxy-proc (get-proxy-process kubernetes--global-process-ledger
                                         '("--port" "1111")))

(cl-defmethod get-proxy-process ((ledger kubernetes--process-ledger) &optional args)
  "Get a proxy process from LEDGER with ARGS.

ARGS is a list of flags and values to kubectl proxy.

If there is already a process recorded in the ledger, return that process.
  Otherwise, make a new one, record it in the ledger, and return it."
  (-if-let* ((port-maybe (kubernetes--val-from-arg-list args 'port))
             (port (and port-maybe (string-to-number port-maybe)))
             (proxy-proc-record (oref ledger proxy))
             (same-port-p (or (not port) (= port (oref proxy-proc-record port)))))
      (oref proxy-proc-record process)
    (let* ((port (or port kubernetes-default-proxy-port))
           (proxy-output-buffer (generate-new-buffer
                                 (format "*kubectl proxy<%s>*" port)))
           ;; FIXME: This process needs to be set up such that it cleans itself
           ;; up if the underlying process completes/dies
           (proxy-proc (kubernetes-kubectl
                        kubernetes-props
                        (kubernetes-state)
                        `("proxy" "--port" ,(format "%s" port))
                        nil)))
      (oset ledger proxy (kubernetes--ported-process-record
                          :process proxy-proc
                          :address "localhost"
                          :port port))
      (if (proxy-ready-p ledger)
          proxy-proc
        (kill-process proxy-proc)
        (oset ledger proxy nil)
        (error "Failed to start kubectl proxy")))))

(cl-defmethod get-process-for-resource ((ledger kubernetes--process-ledger) resource)
  "Get polling process for RESOURCE in LEDGER."
  (map-elt (slot-value ledger 'poll-processes) resource))

(cl-defmethod poll-process-live-p ((ledger kubernetes--process-ledger) resource)
  "Determine if the polling process for RESOURCE in LEDGER is
live or not."
  (process-live-p (get-process-for-resource ledger resource)))

;; FIXME: This needs to release the proxy process as well
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
