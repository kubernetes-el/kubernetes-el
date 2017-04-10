;;; kubernetes-ast.el --- Rendering AST.  -*- lexical-binding: t; -*-
;;; Commentary:

;; Implements an interpreter for a simple layout DSL for magit sections.

;;; Code:

(require 'magit)

(defvar kubernetes--render-indentation-width 2)

(defsubst kubernetes--indentation (indent-level)
  (let ((space ?\ ))
    (make-string (* indent-level kubernetes--render-indentation-width) space)))

(defun kubernetes--eval-ast (render-ast &optional indent-level)
  "Evaluate RENDER-AST in the context of the current buffer.

INDENT-LEVEL is the current indentation level at which to render.

Warning: This could blow the stack if the AST gets too deep."
  (let ((indent-level (or indent-level 0)))
    (pcase render-ast

      ;; Core forms

      ((and x (pred stringp))
       (insert (concat (kubernetes--indentation indent-level) x)))

      (`(line ,inner-ast)
       (kubernetes--eval-ast inner-ast indent-level)
       (newline))

      (`(heading ,inner-ast)
       (unless magit-insert-section--current
         (error "Eval AST: Inserting a heading, but not in a section"))
       (magit-insert-heading (with-temp-buffer
                               (save-excursion (kubernetes--eval-ast inner-ast indent-level))
                               (buffer-substring (line-beginning-position) (line-end-position)))))

      (`(section (,sym ,hide) . ,inner)
       (eval `(magit-insert-section (,sym nil ,hide)
                (kubernetes--eval-ast ',inner ,indent-level))))

      (`(padding . ,inner)
       (when inner (error "Eval AST: Padding takes no arguments"))
       (newline))

      (`(propertize ,spec . ,inner-ast)
       (let ((start (point)))
         (kubernetes--eval-ast inner-ast indent-level)
         (add-text-properties start (point) spec)))

      (`(indent . ,inner-ast)
       (kubernetes--eval-ast inner-ast (1+ indent-level)))


      ;; Sugar forms

      (`(key-value ,width ,k ,v)
       (unless (numberp width) (error "Eval AST: key-value width was not a number"))
       (when (< width 0) (error "Eval AST: key-value width was negative"))
       (unless (stringp k) (error "Eval AST: key-value key was not a string"))
       (unless (stringp v) (error "Eval AST: key-value value was not a string"))

       (let* ((fmt-string (concat "%-" (number-to-string width) "s"))
              (str (concat (propertize (format fmt-string (concat k ":")) 'face 'magit-header-line)
                           v)))
         (kubernetes--eval-ast `(line ,str) indent-level)))

      (`(nav-prop ,spec . ,inner-ast)
       (kubernetes--eval-ast `(propertize (kubernetes-nav ,spec)
                                          ,inner-ast)
                             indent-level))

      (`(copy-prop ,copy-str . ,inner-ast)
       (unless (stringp copy-str)
         (error "Eval AST: nav-prop copy-str was not a string"))
       (kubernetes--eval-ast `(propertize (kubernetes-copy ,copy-str)
                                          ,inner-ast)
                             indent-level))

      (`(mark-for-delete . ,inner-ast)
       (let ((pt (point)))
         (kubernetes--eval-ast inner-ast indent-level)
         (let ((end-line (line-number-at-pos)))
           (save-excursion
             (goto-char pt)
             (while (< (line-number-at-pos) end-line)
               (kubernetes--insert-delete-mark-for-line-at-pt (point))
               (forward-line 1))))))

      ((and actions (pred listp))
       (dolist (action actions)
         (kubernetes--eval-ast action indent-level)))


      (x
       (error "Unknown AST form: %s" x)))))

(defun kubernetes--insert-delete-mark-for-line-at-pt (point)
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
