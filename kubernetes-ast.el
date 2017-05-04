;;; kubernetes-ast.el --- Rendering AST.  -*- lexical-binding: t; -*-
;;; Commentary:

;; Implements an interpreter for a simple layout DSL for magit sections.

;;; Code:

(require 'cl-lib)
(require 'magit)
(require 'subr-x)

;; Derived component support.

(defconst kubernetes-ast--components (make-hash-table :test #'eq)
  "A mapping from the name of a component to its interpretation function.

When traversing a rendering AST, any list beginning with a symbol
is interpreted as a component reference.  That symbol is used to
look up an interpretation function in this table.  That function is
applied to any remaining elements of that cons.

The result of a function in this hash-table should be a new
rendering AST, or a string value to be inserted directly.")

(defmacro kubernetes-ast-define-component (name arglist &rest body)
    "Define a rendering component.

NAME is the name of the component, which may thereafter be
referenced directly in rendering ASTs.

ARGLIST is the arguments that must be supplied to construct the
component.

BODY is the definition of the component."
  (declare (indent 2))
  (cl-assert (symbolp name))
  (cl-assert (listp arglist))
  (let ((fname (intern (format "kubernetes-ast--generated--%s" name)))
        (docstring (format "Auto-generated component constructor function.

Creates instances of %s components, which may be referred to as
such in rendering ASTs." name)))
    `(progn
       (cl-defun ,fname ,arglist ,docstring ,@body)
       (puthash ',name #',fname kubernetes-ast--components))))

(kubernetes-ast-define-component line (inner-ast)
  `(,inner-ast
    (padding)))

(kubernetes-ast-define-component key-value (width key value)
  (cl-assert (numberp width) t)
  (cl-assert (<= 0 width) t)
  (cl-assert (stringp key) t)
  (cl-assert (stringp value) t)
  (let* ((fmt-string (concat "%-" (number-to-string width) "s"))
         (str (concat (propertize (format fmt-string (concat key ": ")) 'face 'magit-header-line)
                      value)))
    (unless (string-blank-p (buffer-substring (line-beginning-position) (line-end-position)))
      (newline))
    `(copy-prop ,value (line ,str))))

(kubernetes-ast-define-component nav-prop (spec &rest inner-ast)
  `(propertize (kubernetes-nav ,spec)
               ,inner-ast))

(kubernetes-ast-define-component copy-prop (copy-str &rest inner-ast)
  (cl-assert (stringp copy-str) t)
  `(propertize (kubernetes-copy ,copy-str)
               ,inner-ast))


;; AST interpreter.

(defconst kubernetes-ast--indentation-width 2)
(defconst kubernetes-ast--space ?\ )

(defsubst kubernetes-ast--indentation (indent-level)
  (make-string (* indent-level kubernetes-ast--indentation-width) kubernetes-ast--space))

;; TODO: Rewrite as a loop.
(defun kubernetes-ast-eval (render-ast &optional indent-level)
  "Evaluate RENDER-AST in the context of the current buffer.

INDENT-LEVEL is the current indentation level at which to render.

Warning: This could blow the stack if the AST gets too deep."
  (let ((indent-level (or indent-level 0))
        ;; HACK: Temporarily increase interpreter limits for recursive calls.
        (max-lisp-eval-depth 10000)
        (max-specpdl-size    4000))

    (pcase render-ast

      ;; Core forms

      ((and x (pred stringp))
       (insert (if (string-empty-p (buffer-substring (line-beginning-position) (point)))
                   (concat (kubernetes-ast--indentation indent-level) x)
                 x)))

      (`(heading ,inner-ast)
       (unless magit-insert-section--current
         (error "Eval AST: Inserting a heading, but not in a section"))
       (magit-insert-heading (with-temp-buffer
                               (save-excursion (kubernetes-ast-eval inner-ast indent-level))
                               (buffer-substring (line-beginning-position) (line-end-position)))))

      (`(section (,sym ,hide) . ,inner)
       (eval `(magit-insert-section (,sym nil ,hide)
                (kubernetes-ast-eval ',inner ,indent-level))))

      (`(padding . ,rest)
       (when rest
         (error "Padding takes no arguments"))
       (newline))

      (`(propertize ,spec . ,inner-ast)
       (let ((start (point)))
         (kubernetes-ast-eval inner-ast indent-level)
         (add-text-properties start (point) spec)))

      (`(indent . ,inner-ast)
       (kubernetes-ast-eval inner-ast (1+ indent-level)))

      (`(list . ,items)
       (dolist (item items)
         (let ((item-start (point)))
           (kubernetes-ast-eval `(line ,item) (+ 1 indent-level))
           ;; Insert dash for item.
           (save-excursion
             (goto-char item-start)
             (goto-char (line-beginning-position))
             (skip-chars-forward " ")
             (unless (eq (char-after) ?-)
               (delete-char -2)
               (insert "- "))))
         (delete-char -1)))

      (`(mark-for-delete . ,inner-ast)
       (let ((start (point)))
         (kubernetes-ast-eval inner-ast indent-level)
         (let ((end-line (line-number-at-pos)))
           (save-excursion
             (goto-char start)
             (kubernetes-ast-put-delete-mark-on-line-at-pt (point))
             (while (< (line-number-at-pos) end-line)
               (kubernetes-ast-put-delete-mark-on-line-at-pt (point))
               (forward-line 1))))))


      ;; Interpret components defined in terms of core forms.

      ((and `(,component . ,args)
            (guard component)
            (guard (symbolp component)))
       (if-let (constructor (gethash component kubernetes-ast--components))
           (kubernetes-ast-eval (apply constructor args) indent-level)
         (error "Component not defined: %s" component)))


      ;; Interpret lists of AST types.

      ((and actions (pred listp))
       (dolist (action actions)
         (kubernetes-ast-eval action indent-level)))

      (x
       (error "Unknown AST form: %s" x)))))


;; Special operations.

(defun kubernetes-ast-put-delete-mark-on-line-at-pt (point)
  (save-excursion
    (goto-char point)
    (goto-char (line-beginning-position))
    (let* ((existing-props (text-properties-at (point)))
           (props (append existing-props '(face kubernetes-delete-mark)))
           (mark-str (concat (apply #'propertize "D" props)
                             (apply #'propertize " " existing-props))))
      (cond
       ((member 'kubernetes-delete-mark existing-props)
        nil)
       ((looking-at-p (rx bol space space))
        (delete-char 2)
        (insert mark-str))
       (t
        (insert mark-str))))))

(provide 'kubernetes-ast)

;;; kubernetes-ast.el ends here
