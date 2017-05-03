;;; kubernetes-loading-container.el --- Component for resources that are loaded asynchronously.  -*- lexical-binding: t; -*-
;;; Commentary:

;; A loading container displays placeholder text while a resource is loading in
;; the background.
;;
;; Other components build on this functionality to render differently depending
;; on the loaded state of some resource.

;;; Code:

(require 'kubernetes-ast)

(kubernetes-ast-define-component membership-loading-discriminator (elem vector &key on-loading on-found on-not-found)
  (cond
   ((null vector)
    on-loading)
   ((and vector (seq-contains vector elem))
    on-found)
   (t
    on-not-found)))

(kubernetes-ast-define-component membership-loading-container (elem vector &rest loaded-content)
  `(membership-loading-discriminator
    ,elem ,vector

    :on-loading
    (line (propertize (face kubernetes-progress-indicator) (line "Fetching...")))

    :on-found
    ,loaded-content

    :on-not-found
    (line (propertize (face kubernetes-progress-indicator) "Not found."))))


(kubernetes-ast-define-component emptiness-loading-discriminator (vector &key on-loading on-empty on-populated)
  (cond
   ((and (vectorp vector) (equal 0 (length vector)))
    on-empty)
   (vector
    on-populated)
   (t
    on-loading)))

(kubernetes-ast-define-component loading-container (resource-vector &rest loaded-content)
  `(emptiness-loading-discriminator
    ,resource-vector

    :on-loading
    (propertize (face kubernetes-progress-indicator) (line "Fetching..."))

    :on-empty
    (propertize (face magit-dimmed) (line "None."))

    :on-populated ,loaded-content))

(kubernetes-ast-define-component columnar-loading-container (resource-vector column-header &rest loaded-content)
  `(emptiness-loading-discriminator
    ,resource-vector

    :on-loading
    ((line ,column-header)
     (propertize (face kubernetes-progress-indicator) (line "Fetching...")))

    :on-empty
    (propertize (face magit-dimmed) (line "None."))

    :on-populated
    ((line ,column-header)
     ,@loaded-content)))

(kubernetes-ast-define-component header-with-count (header resource-vector)
  (let ((header (propertize header 'face 'magit-header-line)))
    `(heading
      (emptiness-loading-discriminator
       ,resource-vector
       :on-loading
       ,header
       :on-empty
       ,(concat header " (0)")
       :on-populated
       ,(concat header (format " (%s)" (length resource-vector)))))))

(provide 'kubernetes-loading-container)

;;; kubernetes-loading-container.el ends here
