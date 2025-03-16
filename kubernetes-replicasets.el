;;; kubernetes-replicasets.el --- Rendering for Kubernetes replicasets.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'kubernetes-state)

(kubernetes-state-define-refreshers replicasets)

(provide 'kubernetes-replicasets)

;;; kubernetes-replicasets.el ends here
