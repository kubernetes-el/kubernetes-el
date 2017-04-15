;;; kubernetes-yaml.el --- YAML pretty-printing.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'subr-x)

(require 'kubernetes-ast)
(require 'kubernetes-modes)
(require 'kubernetes-vars)


;; Compile parsed JSON into an AST representation for rendering.

(defun kubernetes-yaml-render (json &optional level)
  "Process some parsed JSON and pretty-print as YAML.

JSON is a parsed JSON value.

LEVEL indentation level to use.  It defaults to 0 if not supplied."
  (let* ((level (or level 0))
         (space (string-to-char " "))
         (indentation (make-string (* level kubernetes-yaml-indentation-width) space))
         (body
          (cond
           ((vectorp json)
            (let* ((list-items (--map (string-trim-left (kubernetes-yaml-render it (1+ level)))
                                      (append json nil)))
                   (separator (concat "\n"
                                      indentation "-" "\n"
                                      indentation "  "))
                   (joined (string-join list-items separator)))
              ;; If this is an empty or singleton list, do not drop.
              (if (<= (length list-items) 1)
                  (concat indentation "- " (string-trim-right joined))
                (concat indentation "- \n"
                        indentation "  " (string-trim-right joined)))))
           ((listp json)
            (let ((entries (--map
                            (-let [(k . v) it]
                              (concat indentation
                                      (propertize (format "%s: " k) 'face 'kubernetes-json-key)
                                      (cond
                                       ((equal t v) "true")
                                       ((equal :json-false v) "false")
                                       ((equal nil v) "null")

                                       ((numberp v)
                                        (number-to-string v))

                                       ((and (stringp v) (string-match-p "\n" v))
                                        (let* ((next-indentation (make-string (* (1+ level) kubernetes-yaml-indentation-width) space))
                                               (indented
                                                (string-join
                                                 (--map (concat next-indentation it) (split-string v "\n"))
                                                 "\n")))
                                          (concat "|-\n" indented)))

                                       ((and (stringp v) (< (length v) kubernetes-yaml-string-drop-threshold))
                                        v)

                                       (t
                                        (concat "\n" (kubernetes-yaml-render v (1+ level)))))))
                            json)))
              (string-join entries "\n")))
           (t
            (format "%s%s" indentation json)))))
    (if (= 0 level)
        (concat (propertize "---\n" 'face 'magit-dimmed) body)
      body)))


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
