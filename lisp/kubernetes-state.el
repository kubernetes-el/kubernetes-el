;;; kubernetes-state.el --- Application state manager.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'kubernetes-kubectl)
(require 'kubernetes-props)

(require 'dash)
(require 'subr-x)

(defconst kubernetes-state-props
  '((kubeconfig-settings . kubernetes-kubectl-kubeconfig-settings))
  "Functions to inject for isolation and testing.")

(defvar kubernetes-state-client-message-processed-functions nil
  "Hook functions run when an update is received from the subprocess.

Each entry must be a function taking a single argument, which is
the parsed s-expression message.")

;; Main state lifecycle.

(defun kubernetes-state-empty ()
  (let ((ht
         (make-hash-table
          ;; Set this to the number of top-level state attrs
          :size 10
          :test #'eq)))
    (puthash 'pods (make-hash-table :test #'eq) ht)
    ht))

(defconst kubernetes-state (kubernetes-state-empty))

(defun kubernetes-state ()
  kubernetes-state)


;; Accessors

(defmacro kubernetes-state-defaccessors (name arglist &rest assertions)
  (declare (indent defun))
  (unless (symbolp name) (error "NAME must be a symbol"))
  (unless (listp arglist) (error "ARGLIST must be a list"))
  (unless (equal 1 (length arglist)) (error "ARGLIST must contain a single binding"))
  `(progn
     (defun ,(intern (format "kubernetes-state-%s" name)) (&optional state)
       (gethash (quote ,name) (or state (kubernetes-state))))

     (defun ,(intern (format "kubernetes-state-set-%s" name)) ,arglist
       ,@assertions
       (puthash (quote ,name) ,(car arglist) (kubernetes-state)))))


(kubernetes-state-defaccessors client-process (process))

(kubernetes-state-defaccessors namespace (ns)
  (unless (stringp ns)
    (error "Namespace was not a string: %S" ns)))

(kubernetes-state-defaccessors context (value)
  (unless (stringp value)
    (error "Context was not a string: %S" value)))

(kubernetes-state-defaccessors user (value)
  (unless (stringp value)
    (error "User was not a string: %S" value)))

(kubernetes-state-defaccessors cluster (value)
  (unless (stringp value)
    (error "Cluster was not a string: %S" value)))

(kubernetes-state-defaccessors pods (pods))

(kubernetes-state-defaccessors updates-received-p (flag))


;; Lifecycle management.

(defun kubernetes-state-clear ()
  (let ((state (kubernetes-state)))
    (--each (hash-table-keys state)
      (let ((value (gethash it state)))
        (if (hash-table-p value)
            (--each (hash-table-keys value) (remhash it value))
          (remhash it (kubernetes-state)))))))

(defun kubernetes-state--marshal-from-kubectl (&optional props)
  (kubernetes-props-bind ([kubeconfig-settings] (or props kubernetes-state-props))
    (let ((settings (kubeconfig-settings)))
      (-when-let ((&alist 'context context) settings)
        (kubernetes-state-set-context context))
      (-when-let ((&alist 'cluster cluster) settings)
        (kubernetes-state-set-cluster cluster))
      (-when-let ((&alist 'user user) settings)
        (kubernetes-state-set-user user))
      (-when-let ((&alist 'namespace namespace) settings)
        (kubernetes-state-set-namespace namespace)))))

(defun kubernetes-state-reset (&optional props)
  (kubernetes-state-clear)
  (kubernetes-state--marshal-from-kubectl props))

(defun kubernetes-state-reset-resources ()
  (let ((pods (kubernetes-state-pods)))
    (--each (hash-table-keys pods) (remhash it pods))))


;; Handle messages from subprocess.

(defun kubernetes-state-handle-client-line (line)
  (let ((changed-p))

    (-let [(message &as &alist 'type type 'operation operation 'data data)
           (condition-case _
               (read line)
             (error
              (error "Malformed sexp from backend: %s" line)))]

      (pcase (list (intern type) (intern operation))
        (`(pod upsert)
         (let ((pods-table (kubernetes-state-pods))
               (upserts (append data nil)))
           (when upserts (setq changed-p t))
           (--each upserts
             (let ((key (intern (kubernetes-state--resource-name it))))
               (puthash key it pods-table)))))

        (`(pod delete)
         (let ((pods-table (kubernetes-state-pods))
               (deletes (append data nil)))
           (when deletes (setq changed-p t))
           (--each deletes
             (let ((key (intern (kubernetes-state--resource-name it))))
               (remhash key pods-table)))))

        (x
         (error "Unknown type and operation: %s" (prin1-to-string x))))

      (when changed-p
        (run-hook-with-args 'kubernetes-state-client-message-processed-functions message)))))

(defun kubernetes-state--resource-name (resource)
  (-let [(&alist 'metadata (&alist 'name name)) resource]
    name))


(provide 'kubernetes-state)

;;; kubernetes-state.el ends here
