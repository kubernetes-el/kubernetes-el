;;; kubernetes-events.el --- Kubernetes events integration  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)

(require 'kubernetes-ast)
(require 'kubernetes-commands)
(require 'kubernetes-core)
(require 'kubernetes-kubectl)
(require 'kubernetes-loading-container)
(require 'kubernetes-state)
(require 'kubernetes-utils)
(require 'kubernetes-vars)
(require 'kubernetes-yaml)

;; Column Headings - Reduced Age column width
(defconst kubernetes-events--column-format "%-10s %-8s %-10s %-15s %-15s %-12s %s"
  "Format string for events table.")

(defconst kubernetes-events--column-labels "Time Age Type Reason Object Kind Message"
  "Column labels for events table.")

;; Message line width for multiline display
(defconst kubernetes-events--message-width 100
  "Width for wrapping messages in the events buffer.")

;; Mode map
(defvar kubernetes-events-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "g") #'kubernetes-events-refresh)
    (define-key keymap (kbd "q") #'kubernetes-events-quit)
    (define-key keymap (kbd "RET") #'kubernetes-events-inspect-event)
    (define-key keymap (kbd "m") #'kubernetes-events-view-message)
    keymap)
  "Keymap for `kubernetes-events-mode'.")

;; Supporting Functions

(defun kubernetes-events--calc-time-diff (timestamp)
  "Calculate a human-readable time difference string from TIMESTAMP to now."
  (let* ((parsed (kubernetes-utils-parse-utc-timestamp timestamp))
         (time-diff (time-subtract (current-time) (apply #'encode-time parsed)))
         (seconds (floor (time-to-seconds time-diff)))
         (minutes (floor (/ seconds 60)))
         (hours (floor (/ minutes 60)))
         (days (floor (/ hours 24))))
    (cond
     ((< seconds 60) (format "%ds" seconds))
     ((< minutes 60) (format "%dm" minutes))
     ((< hours 24) (format "%dh" hours))
     (t (format "%dd" days)))))

(defun kubernetes-events--format-time (time)
  "Format the TIME string for display in an event line."
  (let* ((parsed (kubernetes-utils-parse-utc-timestamp time))
         (encoded (apply #'encode-time parsed)))
    (format-time-string "%H:%M:%S" encoded)))

(defun kubernetes-events--format-type (type)
  "Format the event TYPE with appropriate face."
  (propertize type 'face
              (cond
               ((string= type "Warning") 'error)
               ((string= type "Normal") 'kubernetes-dimmed)
               (t 'default))))

(defun kubernetes-events--read-resource-if-needed (state)
  "Read a resource from the minibuffer if none is at point using STATE.
Returns a cons cell of (type . name)."
  (if-let ((resource-info (kubernetes-utils-get-resource-info-at-point)))
      resource-info
    (message "No resource at point. Showing all events.")
    nil))

(defun kubernetes-events--generate-buffer-name (&optional resource-type resource-name)
  "Generate a buffer name for events.
If RESOURCE-TYPE and RESOURCE-NAME are provided, include them in the buffer name."
  (if (and resource-type resource-name)
      (format "*kubernetes events: %s/%s/%s*"
              (kubernetes-state--get (kubernetes-state) 'current-namespace)
              resource-type resource-name)
    (format "*kubernetes events: %s*"
            (kubernetes-state--get (kubernetes-state) 'current-namespace))))

(defun kubernetes-events--wrap-text (text width)
  "Wrap TEXT to WIDTH characters."
  (with-temp-buffer
    (insert text)
    (let ((fill-column width))
      (fill-region (point-min) (point-max)))
    (buffer-string)))

(defun kubernetes-events--redraw-buffer (events &optional title)
  "Redraw the events buffer with EVENTS data.
TITLE is an optional title for the buffer."
  (with-current-buffer (current-buffer)
    (let ((inhibit-read-only t)
          (labels (split-string kubernetes-events--column-labels)))
      (erase-buffer)
      (kubernetes-events-mode)

      ;; Insert title if provided
      (when title
        (insert (propertize title 'face 'magit-section-heading))
        (insert "\n\n"))

      ;; Insert header with explicit column positioning
      (insert (propertize
               (format (concat "%-10s %-8s %-10s %-15s %-15s %-12s %s")
                       (nth 0 labels) (nth 1 labels) (nth 2 labels)
                       (nth 3 labels) (nth 4 labels) (nth 5 labels) (nth 6 labels))
               'face 'magit-section-heading))
      (insert "\n\n")

      ;; Insert events
      (if events
          (let* ((items (alist-get 'items events))
                 ;; Convert vector to list if needed
                 (items-list (if (vectorp items) (append items nil) items))
                 (sorted-events (seq-sort (lambda (a b)
                                           (string> (alist-get 'lastTimestamp a "")
                                                   (alist-get 'lastTimestamp b "")))
                                         items-list))
                 (count 0))
            (dolist (event sorted-events)
              (kubernetes-events--insert-event event)
              (insert "\n")
              (setq count (1+ count)))
            (goto-char (point-min))
            (when title
              (forward-line 1)
              (beginning-of-line)
              (insert (format "Found %d events\n" count))
              (goto-char (point-min))))
        (insert (propertize "No events found" 'face 'kubernetes-dimmed)))
      (goto-char (point-min)))))

(defun kubernetes-events--insert-event (event)
  "Insert EVENT line into the current buffer."
  (-let* (((&alist 'type type
                  'reason reason
                  'message message
                  'metadata (&alist 'creationTimestamp time
                                   'name event-name)
                  'involvedObject (&alist 'name obj-name
                                          'kind kind
                                          'fieldPath field-path))
           event)
          (fmt-list (split-string kubernetes-events--column-format))
          (time-str (kubernetes-events--format-time time))
          (age-str (kubernetes-events--calc-time-diff time))
          (obj-str (if field-path
                       (concat obj-name "/" (car (last (split-string field-path "/"))))
                     obj-name))
          ;; Split message into lines and wrap each to fit width
          (wrapped-message (kubernetes-events--wrap-text message kubernetes-events--message-width))
          (message-lines (split-string wrapped-message "\n"))
          (first-line (car message-lines))
          (remaining-lines (cdr message-lines))
          ;; Calculate indentation for continuation lines - sum of all column widths + spaces
          (indent-width (+ 10 1 8 1 10 1 15 1 15 1 12 1))  ; Column widths + spaces between
          )

    (let ((start (point)))
      ;; Insert the first line with all columns
      (insert
       ;; Time
       (propertize (format (pop fmt-list) time-str) 'face 'kubernetes-dimmed)
       " "
       ;; Age
       (propertize (format (pop fmt-list) age-str) 'face 'kubernetes-dimmed)
       " "
       ;; Type
       (format (pop fmt-list) (kubernetes-events--format-type type))
       " "
       ;; Reason
       (propertize (format (pop fmt-list) reason) 'face 'kubernetes-dimmed)
       " "
       ;; Object - Truncate to 13 characters to fit column width
       (propertize (format (pop fmt-list) (s-truncate 13 obj-str)) 'face 'kubernetes-resource-name)
       " "
       ;; Kind
       (propertize (format (pop fmt-list) kind) 'face 'kubernetes-dimmed)
       " "
       ;; Message - First line
       (propertize (format (pop fmt-list) first-line) 'face 'default))

      ;; Add any continuation lines with proper indentation
      (when remaining-lines
        (let ((indent-str (concat "\n" (make-string indent-width ?\s))))
          (dolist (line remaining-lines)
            (insert indent-str (propertize line 'face 'default)))))

      ;; Store data as properties for use with RET key
      (add-text-properties
       start (point)
       `(event-data ,event
                   kubernetes-nav ,(if (string= kind "Pod")
                                       `(:pod-name ,obj-name)
                                     `(,(intern (format ":%s-name" (downcase kind))) ,obj-name)))))))

;; Mode definition

;;;###autoload
(define-derived-mode kubernetes-events-mode kubernetes-mode "Kubernetes Events"
  "Mode for displaying Kubernetes events.

\\<kubernetes-events-mode-map>\
Type \\[kubernetes-events-refresh] to refresh the events list.
Type \\[kubernetes-events-quit] to quit the events buffer.
Type \\[kubernetes-events-inspect-event] to view details of the event at point.
Type \\[kubernetes-events-view-message] to view the full message of the event at point.

\\{kubernetes-events-mode-map}"
  :group 'kubernetes
  (buffer-disable-undo)
  (setq truncate-lines nil  ; Allow lines to wrap
        buffer-read-only t))

;; Interactive commands

;;;###autoload
(defun kubernetes-events-fetch (&optional resource-type resource-name args)
  "Display events in a new buffer.
If RESOURCE-TYPE and RESOURCE-NAME are provided, filter events for that resource.
ARGS are additional arguments to pass to kubectl."
  (interactive)
  (let* ((buffer-name (kubernetes-events--generate-buffer-name resource-type resource-name))
         (base-args (list "events" "-o" "json"))
         (filter-args (when (and resource-type resource-name)
                        (list "--for"
                              (format "%s/%s"
                                      (capitalize resource-type) resource-name))))
         (cmd-args (append base-args filter-args args))
         (title (if (and resource-type resource-name)
                    (format "Events for %s/%s" (capitalize resource-type) resource-name)
                  "All cluster events")))

    (with-current-buffer (get-buffer-create buffer-name)
      (kubernetes-events-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Fetching %s...\n" title))))

    (kubernetes-kubectl (kubernetes-state)
                     cmd-args
                     (lambda (buf)
                       (with-current-buffer buf
                         (let ((content (buffer-string)))
                           ;; Check if response is actually JSON
                           (condition-case err
                               (let ((events (json-read-from-string content)))
                                 (with-current-buffer (get-buffer buffer-name)
                                   (kubernetes-events--redraw-buffer events title)))
                             ;; If JSON parsing fails, treat as "no events" message
                             (json-error
                              (with-current-buffer (get-buffer buffer-name)
                                (kubernetes-events--redraw-buffer nil title)))))))
                     (lambda (_)
                       (with-current-buffer (get-buffer buffer-name)
                         (kubernetes-events--redraw-buffer nil title))))

    (select-window (display-buffer (get-buffer-create buffer-name)))))

;;;###autoload
(defun kubernetes-events-fetch-all (&optional args state)
  "Show all events in the cluster.
ARGS are additional arguments to pass to kubectl.
STATE is the current application state."
  (interactive
   (list (transient-args 'kubernetes-events)
         (kubernetes-state)))
  (let ((resource-info (kubernetes-events--read-resource-if-needed state)))
    (if resource-info
        (kubernetes-events-fetch (car resource-info) (cdr resource-info) args)
      (kubernetes-events-fetch nil nil args))))

;;;###autoload
(defun kubernetes-events-refresh ()
  "Refresh the events buffer."
  (interactive)
  (let ((buf (current-buffer)))
    (when (string-match-p "\\*kubernetes events.*\\*" (buffer-name buf))
      (with-current-buffer buf
        (let ((inhibit-read-only t)
              (title (save-excursion
                       (goto-char (point-min))
                       (when (looking-at "\\([^\n]+\\)")
                         (match-string 1))))
              (resource-info (when (string-match "\\*kubernetes events: \\(?:[^/]+\\)/\\([^/]+\\)/\\([^*]+\\)\\*" (buffer-name buf))
                               (cons (match-string 1 (buffer-name buf))
                                     (match-string 2 (buffer-name buf))))))
          (erase-buffer)
          (insert "Refreshing events...")
          (if resource-info
              (let ((resource-type (car resource-info))
                    (resource-name (cdr resource-info)))
                (kubernetes-events-fetch resource-type resource-name nil))
            (kubernetes-events-fetch nil nil nil)))))))

;;;###autoload
(defun kubernetes-events-quit ()
  "Quit the events buffer."
  (interactive)
  (let ((buf (current-buffer)))
    (when (string-match-p "\\*kubernetes events.*\\*" (buffer-name buf))
      (when-let (window (get-buffer-window buf))
        (quit-window nil window)))))

;;;###autoload
(defun kubernetes-events-inspect-event ()
  "Inspect the event at point."
  (interactive)
  (if-let ((event (get-text-property (point) 'event-data)))
      (select-window
       (display-buffer
        (kubernetes-yaml-make-buffer "*Kubernetes Event*" event)))
    (message "No event at point")))

;;;###autoload
(defun kubernetes-events-view-message ()
  "View the full message of the event at point in a separate buffer."
  (interactive)
  (if-let ((event (get-text-property (point) 'event-data)))
      (let* ((message (alist-get 'message event))
             (buf (get-buffer-create "*Kubernetes Event Message*")))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert message)
            (goto-char (point-min))
            (view-mode)
            (select-window (display-buffer buf)))))
    (message "No event at point")))

;;;###autoload
(defun kubernetes-events-view-resource-at-point ()
  "View the resource referenced by the event at point."
  (interactive)
  (when-let ((nav-prop (get-text-property (point) 'kubernetes-nav)))
    (kubernetes-nav-pop-to-resource-at-point)))

;; Handle vector or list of items properly
(defun kubernetes-events--convert-items-if-needed (items)
  "Convert ITEMS to a list if it's a vector."
  (if (vectorp items)
      (append items nil)
    items))

(transient-define-argument kubernetes-events:--types ()
  :description "Event types"
  :class 'transient-switches
  :key "-t"
  :argument-format "--types=%s"
  :argument-regexp "^--types=\\(Normal\\|Warning\\|Normal,Warning\\)$"
  :choices '("Normal,Warning" "Warning" "Normal"))

;; Update the transient prefix to include --types option
(transient-define-prefix kubernetes-events ()
  "Fetch Kubernetes events."
  [["Options"
    ("-a" "All namespaces" "--all-namespaces")
    ("-n" "Namespace" "--namespace=" :reader kubernetes-read-namespace)
     (kubernetes-events:--types)]]
  [["Actions"
    ("T" "Events" kubernetes-events-fetch-all)]])


(provide 'kubernetes-events)

;;; kubernetes-events.el ends here
