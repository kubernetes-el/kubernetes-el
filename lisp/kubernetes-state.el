;;; kubernetes-state.el --- Application state manager.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(provide 'kubernetes-state)

(defun kubernetes-state-empty ()
  (make-hash-table
   ;; Set this to the number of top-level state attrs
   :size 10
   :test #'eq))

(defvar kubernetes-state (kubernetes-state-empty))


;; Accessors

(defmacro kubernetes-state-defaccessors (name arglist &rest assertions)
  (declare (indent defun))
  (unless (symbolp name) (error "NAME must be a symbol"))
  (unless (listp arglist) (error "ARGLIST must be a list"))
  (unless (equal 1 (length arglist)) (error "ARGLIST must contain a single binding"))
  `(progn
     (defun ,(intern (format "kubernetes-state-%s" name)) ()
       (gethash (quote ,name) kubernetes-state))

     (defun ,(intern (format "kubernetes-state-set-%s" name)) ,arglist
       ,@assertions
       (puthash (quote ,name) ,(car arglist) kubernetes-state))))


(kubernetes-state-defaccessors client-process (process))

(kubernetes-state-defaccessors namespace (ns)
  (unless (stringp ns)
    (error "Namespace was not a string: %S" ns)))


;;; kubernetes-state.el ends here
