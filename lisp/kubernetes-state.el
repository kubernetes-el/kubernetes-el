;;; kubernetes-state.el --- Application state manager.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)

(provide 'kubernetes-state)

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

(defun kubernetes-state-clear ()
  (remhash 'namespace (kubernetes-state))
  (let ((pods-list (kubernetes-state-pods)))
    (dolist (key (hash-table-keys pods-list))
      (remhash key pods-list))))

(defvar kubernetes-state-client-message-processed-functions nil
  "Hook functions run when an update is received from the subprocess.

Each entry must be a function taking a single argument, which is
the parsed s-expression message.")


;; Handle messages from subprocess.

(defun kubernetes-state-handle-client-line (line)
  (-let [(message &as &alist 'type type 'operation operation 'data data)
         (condition-case _
             (read line)
           (error
            (error "Malformed sexp from backend: %s" line)))]

    (pcase (list (intern type) (intern operation))
      (`(pod upsert)
       (let ((pods-table (kubernetes-state-pods)))
         (--each (append data nil)
           (let ((key (intern (kubernetes-state--resource-name it))))
             (puthash key it pods-table)))))

      (`(pod delete)
       (let ((pods-table (kubernetes-state-pods)))
         (--each (append data nil)
           (let ((key (intern (kubernetes-state--resource-name it))))
             (remhash key pods-table)))))

      (x
       (error "Unknown type and operation: %s" (prin1-to-string x))))
    (run-hook-with-args 'kubernetes-state-client-message-processed-functions message)))

(defun kubernetes-state--resource-name (resource)
  (-let [(&alist 'metadata (&alist 'name name)) resource]
    name))


;; Accessors

(defmacro kubernetes-state-defaccessors (name arglist &rest assertions)
  (declare (indent defun))
  (unless (symbolp name) (error "NAME must be a symbol"))
  (unless (listp arglist) (error "ARGLIST must be a list"))
  (unless (equal 1 (length arglist)) (error "ARGLIST must contain a single binding"))
  `(progn
     (defun ,(intern (format "kubernetes-state-%s" name)) ()
       (gethash (quote ,name) (kubernetes-state)))

     (defun ,(intern (format "kubernetes-state-set-%s" name)) ,arglist
       ,@assertions
       (puthash (quote ,name) ,(car arglist) (kubernetes-state)))))


(kubernetes-state-defaccessors client-process (process))

(kubernetes-state-defaccessors namespace (ns)
  (unless (stringp ns)
    (error "Namespace was not a string: %S" ns)))

(kubernetes-state-defaccessors pods (pods))


;;; kubernetes-state.el ends here
