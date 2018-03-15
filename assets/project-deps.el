;;; project-deps.el --- Generate Graphviz dot file for deps.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 'f)
(require 'subr-x)

(defvar project-deps--repo-root (locate-dominating-file default-directory ".git"))

(defconst project-deps--excluded-from-graph
  '("kubernetes-props"
    "kubernetes-state"
    "kubernetes-modes"
    "kubernetes-evil"))

(defconst project-deps--srcs (f-files project-deps--repo-root (lambda (it)
                                        (and (f-ext? it "el")
                                             (-none? (lambda (s) (string-match-p s it))
                                                     project-deps--excluded-from-graph)))))

(defconst project-deps--dot-file (f-join project-deps--repo-root "assets" "project-deps.dot"))
(defconst project-deps--png-file (f-join project-deps--repo-root "assets" "project-deps.png"))

(defconst project-deps--format-string (string-trim-left "
digraph {
    rankdir=LR;
    edge [color=gray];

    {rank=same; configmaps contexts errors ingress jobs namespaces pods secrets services};
    {rank=same; ast process popups};

%s
}
"))

(defconst project-deps--match-kubernetes-requires
  (rx bol (* space)
      "(require" (+ space) "'"
      (group-n 1 "kubernetes-" (+? nonl))
      ")"))

(defun project-deps--requires-in-file (file)
  (let* ((str (split-string (f-read-text file) "\n"))
         (requires
          (--keep
           (when (string-match project-deps--match-kubernetes-requires it)
             (match-string 1 it))
           str)))

    (-difference requires project-deps--excluded-from-graph)))

(defun project-deps--render-attrs (file)
  (let ((source (string-remove-prefix "kubernetes-" (f-filename (f-no-ext file))))
        (url (f-join ".." file)))
    (format "    %s [headlabel=\"%s\" URL=\"%s\"];"
            source
            file
            url)))

(defun project-deps--render-deps (file deps)
  (let ((source (string-remove-prefix "kubernetes-" (f-filename (f-no-ext file))))
        (targets (string-join (sort (--map (string-remove-prefix "kubernetes-" (f-no-ext (f-filename it))) deps)
                                    #'string<) " ")))
    (format "    %s -> { %s };" source targets)))

(defun project-deps-generate ()
  "Update the dot diagram of project dependencies."
  (interactive)
  (let* ((statements
          (-flatten
           (list
            (--map (project-deps--render-attrs it) project-deps--srcs)
            (--keep (when-let (deps (project-deps--requires-in-file it))
                      (project-deps--render-deps it deps))
                    project-deps--srcs))))
         (body (string-join statements "\n"))
         (str (format project-deps--format-string body)))
    (f-write-text str 'utf-8 project-deps--dot-file)
    (shell-command
     (format "dot -Tpng %s -o %s"
             project-deps--dot-file
             project-deps--png-file))))

(provide 'project-deps)

;;; project-deps.el ends here
