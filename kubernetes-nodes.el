;;; kubernetes-nodes.el --- Rendering for Kubernetes nodes  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 'seq)

(require 'kubernetes-ast)
(require 'kubernetes-loading-container)
(require 'kubernetes-modes)
(require 'kubernetes-props)
(require 'kubernetes-state)
(require 'kubernetes-utils)
(require 'kubernetes-yaml)


;; Components

(defconst kubernetes-nodes--column-heading
  ["%-45s %-10s %-9s %-4s %8s" "Name Status Roles Age Version"])

(kubernetes-ast-define-component node-detail (node)
  (-let (((&alist 'metadata (&alist 'name)
                  'status (&alist 'addresses 'nodeInfo))
          node))
    `((key-value 12 "Name" ,name)
      ,@(mapcar
         (lambda (addr)
           (-let [(&alist 'address 'type) addr]
             `(key-value 12 ,type ,address)))
         addresses)
      ,@(mapcar
         (lambda (pair)
           (cl-destructuring-bind (key . val) pair
             `(key-value 12 ,(symbol-name key) ,val)))
         nodeInfo))))

(kubernetes-ast-define-component node-line (state node)
  (-let* ((current-time (kubernetes-state-current-time state))
          ((&alist 'metadata (&alist 'name 'labels 'creationTimestamp)
                   'status (&alist 'conditions
                                   'nodeInfo (&alist 'kubeProxyVersion)))
           node)
          ([fmt] kubernetes-nodes--column-heading)
          (list-fmt (split-string fmt))
          (type (or (seq-some (lambda (x) (when (string= (alist-get 'status x) "True")
                                            (alist-get 'type x)))
                              conditions)
                    "Unknown"))
          (str
           (concat
            ;; Name
            (format (pop list-fmt) (kubernetes-utils-ellipsize name 45))
            " "
            ;; Status
            (let ((s (kubernetes-utils-ellipsize type 10)))
              (format (pop list-fmt)
                      (if (string-match-p "running" type)
                          (propertize s 'face 'magit-dimmed)
                        s)))
            " "
            ;; Roles
            (format (pop list-fmt)
                    (kubernetes-utils-ellipsize
                     (or
                      (seq-some (lambda (x)
                                  (when (string-match
                                         "node-role.kubernetes.io/\\(.+\\)$"
                                         x)
                                    (match-string 1 x)))
                                (mapcar (lambda (x) (symbol-name (car x))) labels))
                      "<none>")
                     8))
            " "
            ;; Age
            (let ((start (apply #'encode-time (kubernetes-utils-parse-utc-timestamp creationTimestamp))))
              (propertize (format (pop list-fmt) (kubernetes-utils-time-diff-string start current-time))
                          'face 'magit-dimmed))
            " "
            ;; Version
            (format (pop list-fmt)
                    (propertize (kubernetes-utils-ellipsize kubeProxyVersion 8)
                                'face 'magit-dimmed))))
          (str (cond
                ((string-match-p "ready" type) str)
                (t (propertize str 'face 'warning))))
          (line `(line ,str)))
    `(nav-prop (:node-name ,name)
               (copy-prop ,name ,line))))

(kubernetes-ast-define-component node (state node)
  `(section (,(intern (kubernetes-state-resource-name node)) t)
            (heading (node-line ,state ,node))
            (indent
             (section (details nil)
                      (node-detail ,node)
                      (padding)))))

(kubernetes-ast-define-component nodes-list (state &optional hidden)
  (-let (((&alist 'items nodes) (kubernetes-state-nodes state))
         ([fmt labels] kubernetes-nodes--column-heading))
    `(section (nodes-container ,hidden)
              (header-with-count "Nodes" ,nodes)
              (indent
               (columnar-loading-container ,nodes
                                           ,(propertize
                                             (apply #'format fmt (split-string labels))
                                             'face
                                             'magit-section-heading)
                                           ,@(--map `(node ,state ,it) nodes)))
              (padding))))

;; Requests and state management

(kubernetes-state-define-refreshers nodes)

;; Interactive commands

(defun kubernetes-nodes--read-name (state)
  "Read a node name from the user.

STATE is the current application state.

Update the node state if it not set yet."
  (-let* (((&alist 'items nodes)
           (or (kubernetes-state-nodes state)
               (progn
                 (message "Getting nodes...")
                 (let ((response (kubernetes-kubectl-await-on-async
                                  kubernetes-props state
                                  #'kubernetes-kubectl-get-nodes)))
                   (kubernetes-state-update-nodes response)
                   response))))
          (nodes (append nodes nil))
          (names (-map #'kubernetes-state-resource-name nodes)))
    (completing-read "Node: " names nil t)))

;;;###autoload
(defun kubernetes-display-node (node-name state)
  "Display information for a node in a new window.

STATE is the current application state.

NODE-NAME is the name of the node to display."
  (interactive (let ((state (kubernetes-state)))
                 (list (kubernetes-nodes--read-name state) state)))
  (if-let (node (kubernetes-state-lookup-node node-name state))
      (select-window
       (display-buffer
        (kubernetes-yaml-make-buffer kubernetes-display-node-buffer-name node)))
    (error "Unknown node: %s" node-name)))

(provide 'kubernetes-nodes)

;;; kubernetes-nodes.el ends here
