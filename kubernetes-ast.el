;;; kubernetes-ast.el --- Rendering AST.  -*- lexical-binding: t; -*-
;;; Commentary:

;; Implements an interpreter for a simple layout DSL for magit sections.

;;; Code:

(require 'cl-lib)
(require 'magit)

(defconst kubernetes-ast--indentation-width 2)
(defconst kubernetes-ast--space ?\ )

(defsubst kubernetes-ast--indentation (indent-level)
  (make-string (* indent-level kubernetes-ast--indentation-width) kubernetes-ast--space))

(defun kubernetes-ast-eval (render-ast &optional indent-level)
  "Evaluate RENDER-AST in the context of the current buffer.

INDENT-LEVEL is the current indentation level at which to render.

Warning: This could blow the stack if the AST gets too deep."
  (let ((indent-level (or indent-level 0)))
    (pcase render-ast

      ;; Core forms

      ((and x (pred stringp))
       (insert (concat (kubernetes-ast--indentation indent-level) x)))

      (`(line ,inner-ast)
       (kubernetes-ast-eval inner-ast indent-level)
       (newline))

      (`(heading ,inner-ast)
       (unless magit-insert-section--current
         (error "Eval AST: Inserting a heading, but not in a section"))
       (magit-insert-heading (with-temp-buffer
                               (save-excursion (kubernetes-ast-eval inner-ast indent-level))
                               (buffer-substring (line-beginning-position) (line-end-position)))))

      (`(section (,sym ,hide) . ,inner)
       (eval `(magit-insert-section (,sym nil ,hide)
                (kubernetes-ast-eval ',inner ,indent-level))))

      (`(padding . ,inner)
       (when inner (error "Eval AST: Padding takes no arguments"))
       (newline))

      (`(propertize ,spec . ,inner-ast)
       (let ((start (point)))
         (kubernetes-ast-eval inner-ast indent-level)
         (add-text-properties start (point) spec)))

      (`(indent . ,inner-ast)
       (kubernetes-ast-eval inner-ast (1+ indent-level)))


      ;; Sugar forms

      (`(key-value ,width ,k ,v)
       (cl-assert (numberp width))
       (cl-assert (<= 0 width))
       (cl-assert (stringp k))
       (cl-assert (stringp v))

       (let* ((fmt-string (concat "%-" (number-to-string width) "s"))
              (str (concat (propertize (format fmt-string (concat k ":")) 'face 'magit-header-line)
                           v)))
         (kubernetes-ast-eval `(line ,str) indent-level)))

      (`(nav-prop ,spec . ,inner-ast)
       (kubernetes-ast-eval `(propertize (kubernetes-nav ,spec)
                           ,inner-ast)
              indent-level))

      (`(copy-prop ,copy-str . ,inner-ast)
       (cl-assert (stringp copy-str))
       (kubernetes-ast-eval `(propertize (kubernetes-copy ,copy-str)
                           ,inner-ast)
              indent-level))

      (`(mark-for-delete . ,inner-ast)
       (let ((pt (point)))
         (kubernetes-ast-eval inner-ast indent-level)
         (let ((end-line (line-number-at-pos)))
           (save-excursion
             (goto-char pt)
             (while (< (line-number-at-pos) end-line)
               (kubernetes-ast-put-delete-mark-on-line-at-pt (point))
               (forward-line 1))))))

      ((and actions (pred listp))
       (dolist (action actions)
         (kubernetes-ast-eval action indent-level)))


      (x
       (error "Unknown AST form: %s" x)))))

(defun kubernetes-ast-put-delete-mark-on-line-at-pt (point)
  (save-excursion
    (goto-char point)
    (goto-char (line-beginning-position))
    (let* ((existing-props (text-properties-at (point)))
           (props (append existing-props '(face kubernetes-delete-mark)))
           (mark-str (concat (apply #'propertize "D" props)
                             (apply #'propertize " " existing-props))))
      (cond
       ((looking-at-p (rx bol space space))
        (delete-char 2)
        (insert mark-str))
       (t
        (insert mark-str))))))

(provide 'kubernetes-ast)

;;; kubernetes-ast.el ends here
