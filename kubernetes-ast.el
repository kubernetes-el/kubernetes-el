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


;; AST interpreter.

(defconst kubernetes-ast--indentation-width 2)
(defconst kubernetes-ast--space ?\ )

(defsubst kubernetes-ast--indentation (indent-level)
  (make-string (* indent-level kubernetes-ast--indentation-width) kubernetes-ast--space))

(defsubst kubernetes-ast--eval-string (s indent-level)
  (let ((value (if (string-empty-p (buffer-substring (line-beginning-position) (point)))
                   (concat (kubernetes-ast--indentation indent-level) s)
                 s)))
    (insert value)))

(defsubst kubernetes-ast--finalize-heading (start-pos)
  ;; This implementation is adapted from `magit-insert-heading'.

  ;; Apply heading face if no other face is set.
  (let ((heading (buffer-substring start-pos (line-end-position))))
    (unless (next-single-property-change 0 'face (concat "0" heading))
      (add-text-properties start-pos (point) '(face magit-section-heading))))
  (unless (bolp)
    (insert ?\n))

  ;; Update containing section to point to this heading.
  (setf (oref magit-insert-section--current content) (point-marker)))

(defsubst kubernetes-ast--finalize-delete-marks (start-pos)
  (let ((end-line (line-number-at-pos)))
    (save-excursion
      (goto-char start-pos)
      (kubernetes-ast-put-delete-mark-on-line-at-pt (point))
      (while (< (line-number-at-pos) end-line)
        (kubernetes-ast-put-delete-mark-on-line-at-pt (point))
        (forward-line 1)))))

(defsubst kubernetes-ast--finalize-list-item (start-pos)
  (save-excursion
    (goto-char start-pos)
    (goto-char (line-beginning-position))
    (skip-chars-forward " ")
    (unless (eq (char-after) ?-)
      (delete-char -2)
      (insert "- "))))

(defun kubernetes-ast--append-sentinel (instructions sentinel)
  (append (list instructions) (list sentinel)))

(defun kubernetes-ast-eval (ast &optional indent-level)
  "Evaluate AST as a set of instructions for inserting text into the current buffer."

  ;; The evaluator is implemented as a loop over an instruction stack. The
  ;; `instruction-stack' variable is a stack of AST instructions, the head of
  ;; which is the instruction to interpret. Its initial value is set to the
  ;; input to this function. After an instruction is interpreted, the item at
  ;; the top of the stack is popped. The loop ends when there are no more
  ;; instructions on the stack.
  ;;
  ;; If nested instructions are encountered in the AST, they are pushed onto the
  ;; stack, generally with a sentinel instruction to restore previous
  ;; interpreter state.

  (let ((instruction-stack (list ast))
        (indent-level (or indent-level 0)))

    (while instruction-stack
      (pcase (car instruction-stack)

        ;; Strings are inserted directly, possibly with indentation.

        ((and (pred stringp) s)
         (kubernetes-ast--eval-string s indent-level)
         (!cdr instruction-stack))

        ;; Padding gets some special error checking to make sure it has no inner
        ;; AST, since I get `padding' and `indent' mixed up all the time.

        ((and `(padding . ,_rest) (guard _rest))
         (error "Padding takes no arguments"))
        (`(padding)
         (newline)
         (!cdr instruction-stack))

        ;; Indentation
        ;;
        ;; The current indentation level is tracked by the interpreter. When an
        ;; `indent' directive is encountered, the indent level is incremented
        ;; and the inner AST is pushed to the stack with a sentinel appended.
        ;; When the sentinel is encountered, the indentation level is decreased.

        (`(indent . ,inner-ast)
         (let ((next (kubernetes-ast--append-sentinel inner-ast 'kubernetes-ast--indent-sentinel)))
           (setq indent-level (1+ indent-level))
           (!cdr instruction-stack)
           (!cons next instruction-stack)))

        (`kubernetes-ast--indent-sentinel
         (setq indent-level (1- indent-level))
         (!cdr instruction-stack))

        ;; Properties
        ;;
        ;; To propertize some inserted text, the inner AST is pushed to the
        ;; stack with a sentinel appended. The sentinel records the properties
        ;; to apply and the start position of the span. Once the sentinel is
        ;; encountered, the end position of the span is known and properties can
        ;; be applied.

        (`(propertize ,spec . ,inner-ast)
         (let ((next (kubernetes-ast--append-sentinel inner-ast `(kubernetes-ast--propertize-sentinel ,(point) ,spec))))
           (!cdr instruction-stack)
           (!cons next instruction-stack)))

        (`(kubernetes-ast--propertize-sentinel ,start ,spec)
         (add-text-properties start (point) spec)
         (!cdr instruction-stack))

        ;; Deletion marks
        ;;
        ;; Deletion marks are applied to every line of the inner AST, so the
        ;; inner AST is pushed to the stack with a sentinel that records the
        ;; start position. Once the sentinel is encountered, the range of lines
        ;; that must be modified is known and the marks are written.

        (`(mark-for-delete . ,inner-ast)
         (let ((next (kubernetes-ast--append-sentinel inner-ast `(kubernetes-ast--mark-for-delete-sentinel . ,(point)))))
           (!cdr instruction-stack)
           (!cons next instruction-stack)))

        (`(kubernetes-ast--mark-for-delete-sentinel . ,start)
         (kubernetes-ast--finalize-delete-marks start)
         (!cdr instruction-stack))

        ;; Bulleted lists
        ;;
        ;; A bulleted list is decomposed into a sequence of instructions, each
        ;; of which tracks its buffer positions using sentinel values.
        ;;
        ;; The bullet group is indented, and each item's start position is
        ;; recorded in a sentinel value. When an item's sentinel is encountered,
        ;; the item's dash is written to the buffer.

        (`(list . ,items)
         (let ((next `(indent ,@(--map `(kubernetes-ast--list-item . ,it) items))))
           (!cdr instruction-stack)
           (!cons next instruction-stack)))

        (`(kubernetes-ast--list-item . ,inner-ast)
         (let ((next (kubernetes-ast--append-sentinel inner-ast `(kubernetes-ast--list-item-sentinel . ,(point)))))
           (!cdr instruction-stack)
           (!cons next instruction-stack)))

        (`(kubernetes-ast--list-item-sentinel . ,start)
         (kubernetes-ast--finalize-list-item start)
         (!cdr instruction-stack))

        ;; Headings
        ;;
        ;; Heading insertion requires interpretation of an inner AST to build
        ;; the heading text. A special sentinel is appended to the inner AST
        ;; that tells the interpreter to finalise the heading after interpreting
        ;; the inner value.

        (`(heading ,inner-ast)
         (unless magit-insert-section--current (error "Eval AST: Inserting a heading, but not in a section"))
         (let ((next (kubernetes-ast--append-sentinel inner-ast `(kubernetes-ast--heading-sentinel . ,(point)))))
           (!cdr instruction-stack)
           (!cons next instruction-stack)))

        (`(kubernetes-ast--heading-sentinel . ,start-pos)
         (kubernetes-ast--finalize-heading start-pos)
         (!cdr instruction-stack))

        ;; Sections
        ;;
        ;; KLUDGE: The section insertion logic in magit has complex state. It's
        ;; easier just to evaluate recursively than try to reproduce that logic
        ;; in the interpreter. This is safe so long as section nesting doesn't
        ;; approach `max-lisp-eval-depth'.

        (`(section (,sym ,hide) . ,inner)
         (!cdr instruction-stack)
         (eval `(magit-insert-section (,sym nil ,hide)
                  (kubernetes-ast-eval ',inner ,indent-level))))

        ;; Custom components
        ;;
        ;; If the current instruction is a list and its head is a symbol, look
        ;; it up in the component definition table. If the lookup succeeds,
        ;; evaluate the component's constructor function to derive an AST, and
        ;; push that AST onto the stack.

        ((and `(,component . ,args)
              (guard component)
              (guard (symbolp component)))
         (!cdr instruction-stack)

         (if-let (constructor (gethash component kubernetes-ast--components))
             (!cons (apply constructor args) instruction-stack)
           (error "Component not defined: %s" component)))

        ;; Lists of instructions
        ;;
        ;; If the list being scrutinised does not begin with a symbol, it is
        ;; assumed to be a sequence of instructions. The items are pushed to the
        ;; stack.

        ((and (pred listp) actions)
         (!cdr instruction-stack)
         (setq instruction-stack (append actions instruction-stack)))

        ;; Heck, you've done the interpreter a frighten.

        (other
         (message "Stack: %s" instruction-stack)
         (error "Unknown AST instruction: %s" other))))))


(provide 'kubernetes-ast)

;;; kubernetes-ast.el ends here
