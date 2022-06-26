;;; kubernetes-process.el --- Process management.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 'map)
(require 'request)
(require 's)
(require 'subr-x)

(require 'kubernetes-core)

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
  instead of 'pod' and so on.")
   (proxy
    :initarg :proxy
    :initform nil
    :documentation "Proxy process. There should only be one of these at a time."))
  "Track Kubernetes processes.")

(cl-defmethod proxy-active-p ((ledger kubernetes--process-ledger))
  "Determine if the proxy process in LEDGER is active or not."
  (-if-let* ((proxy-record (oref ledger proxy))
             (proxy-proc (oref proxy-record :process)))
      (process-live-p proxy-proc)))

(cl-defmethod proxy-ready-p ((ledger kubernetes--process-ledger))
  "Wait for proxy process of LEDGER to be ready for requests.

Returns nil if the proxy has started, but either readyz or livez
  endpoints return non-200."
  (let ((proxy-record (oref ledger proxy)))
    (and (= 200 (wait-on-endpoint proxy-record "readyz"))
         (= 200 (wait-on-endpoint proxy-record "livez")))))

(cl-defmethod get-proxy-process ((ledger kubernetes--process-ledger) &optional args)
  "Get a proxy process from LEDGER with ARGS.

ARGS is a list of flags and values to kubectl proxy.  They can be
either of form '(\"--foo=bar\") or '(\"--foo\" \"bar\").

If there is already a process recorded in the ledger, return that
  process.  Otherwise, make a new one, record it in the ledger,
  and return it."
  (let* ((mk-proxy (lambda (port)
                     (let ((proxy-output-buffer
                            (generate-new-buffer
                             (format "*kubectl proxy<%s>" port)))
                           (proxy-proc (kubernetes-kubectl
                                        (kubernetes-state)
                                        `("proxy" "--port" ,(format "%s" port))
                                        nil)))
                       (oset ledger proxy (kubernetes--ported-process-record
                                           :process proxy-proc
                                           :address "localhost"
                                           :port port))
                       ;; Give the proxy process some time to spin up, so that
                       ;; curl doesn't return error code 7 which to request.el is
                       ;; a "peculiar error"
                       (sleep-for 2)

                       (if (proxy-ready-p ledger)
                           (progn
                             (kubernetes--info "Started proxy server at %s" (base-url (oref ledger proxy)))
                             (kubernetes--redraw-overview-buffer)
                             proxy-proc)
                         (kubernetes--error "Proxy server failed to start; terminating process")
                         (kill-proxy-process ledger)
                         (error "Failed to start kubectl proxy")))))
         (port-maybe (kubernetes--val-from-arg-list args 'port))
         (port-int-maybe (and port-maybe (string-to-number port-maybe))))
    (-if-let (proxy-proc-record (oref ledger proxy))
        (if (or (not port-int-maybe) (= port-int-maybe (oref proxy-proc-record port)))
            (oref proxy-proc-record process)
          (kill-proxy-process ledger)
          (funcall mk-proxy port-int-maybe))
      (funcall mk-proxy (if (not port-maybe)
                            kubernetes-default-proxy-port
                          (string-to-number port-maybe))))))

(cl-defmethod get-process-for-resource ((ledger kubernetes--process-ledger) resource)
  "Get polling process for RESOURCE in LEDGER."
  (map-elt (slot-value ledger 'poll-processes) resource))

(cl-defmethod kill-proxy-process ((ledger kubernetes--process-ledger))
  "Kill the proxy process at LEDGER if one is present and live."
  (when (and (oref ledger proxy) (oref (oref ledger proxy) process))
    (kubernetes-process-kill-quietly (oref (oref ledger proxy) process))
    (kubernetes--info "Terminated proxy server.")
    (kubernetes--redraw-overview-buffer)
    (oset ledger proxy nil)))

(cl-defmacro kubernetes--with-proxy ((&key cleanup) &rest body)
  "Start a Kubernetes proxy server and execute BODY.

If CLEANUP is non-nil, terminate the proxy process immediately after BODY."
  ;; TODO: Refactor `get-proxy-process' to return the ported process record object rather than the raw process itself
  (get-proxy-process kubernetes--global-process-ledger)
  `(let ((res (progn ,@body)))
     (when ,cleanup (kill-proxy-process kubernetes--global-process-ledger))
     res))

(defmacro kubernetes--require-proxy (&rest body)
  "Execute BODY if and only if Kubernetes proxy server already enabled."
  `(if (not (oref kubernetes--global-process-ledger proxy))
      (error "Kubernetes proxy server required but not active; use `kubernetes--with-proxy' or enable manually with `kubernetes-proxy'.")
     ,@body))

(cl-defmethod poll-process-live-p ((ledger kubernetes--process-ledger) resource)
  "Determine liveness of polling process for RESOURCE in LEDGER."
  (process-live-p (get-process-for-resource ledger resource)))

(cl-defmethod release-all ((ledger kubernetes--process-ledger))
  "Release all processes in LEDGER.

Returns the resources for which processes were released."
  (kill-proxy-process ledger)
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

(kubernetes-ast-define-component proxy-status ()
  (let ((proxy-record (oref kubernetes--global-process-ledger proxy)))
    `(key-value
      12
      "Proxy"
      ,(if proxy-record
           (propertize (base-url proxy-record) 'face 'kubernetes-delete-mark)
         (propertize "Disabled" 'face 'kubernetes-dimmed)))))

(provide 'kubernetes-process)

;;; kubernetes-process.el ends here
