;;; kubernetes-yaml.el --- YAML pretty-printing.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 'subr-x)

(require 'kubernetes-ast)
(require 'kubernetes-modes)


;; Compile parsed JSON into an AST representation for rendering.

(defun kubernetes-yaml--render-helper (json)
  (pcase json
    ;; Literals

    ('nil "null")
    ('t "true")
    (:json-false "false")
    ((pred stringp) json)
    ((pred numberp) (number-to-string json))
    ((pred symbolp) (symbol-name json))

    ;; Lists

    ((pred vectorp)
     `(list ,@(seq-map (lambda (it)
                         `(section (item nil) ,(kubernetes-yaml--render-helper it)))
                       json)))

    ;; Objects

    ((pred listp)
     (seq-map (-lambda ((k . v))
                (let ((k (kubernetes-yaml--render-helper k))
                      (v (kubernetes-yaml--render-helper v)))
                  `(section (object-kvp nil)
                            ,(cond
                              ;; Indent multiline strings.
                              ((and (stringp v) (string-match-p "\n" (string-trim-right v)))
                               `(copy-prop ,v
                                           (heading ,(concat (propertize (format "%s:" k) 'face 'magit-section-heading) " |-"))
                                           (indent ,@(--map `(line ,it) (split-string (string-trim-right v) "\n")))))

                              ((stringp v)
                               `(key-value 0 ,k ,v))

                              (t
                               `((heading ,(concat (propertize (format "%s:" k) 'face 'magit-section-heading) " "))
                                 (indent ,v)))))))
              json))

    (_
     (error "Don't know how to render %s" json))))

(defun kubernetes-yaml-render (json)
  "Process some parsed JSON into a YAML AST for rendering."
  `(section (json-root nil)
            ,(kubernetes-yaml--render-helper json)
            (padding)))


;; Drawing utilites

(defun kubernetes-yaml-make-buffer (bufname parsed-json)
  (let ((buf (get-buffer-create bufname)))
    (with-current-buffer buf
      (kubernetes-display-thing-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (kubernetes-ast-eval (kubernetes-yaml-render parsed-json))
        (goto-char (point-min))))
    buf))


(provide 'kubernetes-yaml)

;;; kubernetes-yaml.el ends here
