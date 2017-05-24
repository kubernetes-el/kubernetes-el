;;; kubernetes-props.el --- Utilities for working with props. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'subr-x)

(defmacro kubernetes-props-bind (varlist &rest body)
  "Bind functions from a props alist for direct use in BODY.

VARLIST is a list of 2 elements:

1. A vector of KEYS to bind, and
2. The target ALIST to scrutinise.

The values extracted are bound as functions in BODY, and can be
called directly.

\(fn ([KEYS...] ALIST) &rest BODY)"
  (declare (indent 1) (debug t))
  (unless (listp varlist)
    (error "Binding form must be a list"))
  (unless (equal 2 (length varlist))
    (error "Must provide a list of keys to bind and an alist"))

  (-let [(keys alist) varlist]
    (unless (vectorp keys)
      (error "First item in binding list must be a vector of symbols"))

    (let* ((props (make-symbol "props"))
           (keys (append keys nil))
           (missing-keys (make-symbol "missing-keys"))
           (alist-binder (cons '&alist (--mapcat `((quote ,it) ,it) keys)))
           (flet-binders (--map `(,it (&rest args) (apply ,it args)) keys)))

      (-when-let (invalid (--map (format "%s" it) (-reject #'symbolp keys)))
        (error "Keys must be symbols. Rejected: [%s]" invalid))

      `(let ((,props ,alist))
         (unless (listp ,props)
           (error "Attempted to bind props on a non-list object: %s" ,props))
         (-when-let (,missing-keys (-map #'symbol-name (-difference (quote ,keys) (-map #'car ,props))))
           (error "Props were missing the following keys: [%s]" (string-join ,missing-keys " ")))
         (-let [,alist-binder ,props]
           (cl-flet ,flet-binders
             ,@body))))))


(provide 'kubernetes-props)

;;; kubernetes-props.el ends here
