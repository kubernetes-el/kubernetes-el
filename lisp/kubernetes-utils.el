;;; kubernetes-utils.el --- Common utils for Kubernetes.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)

(require 'kubernetes-state)

(defun kubernetes-parse-utc-timestamp (timestamp)
  "Parse TIMESTAMP string from the API into the representation used by Emacs."
  (let ((parsed (parse-time-string
                 (->> timestamp
                      (replace-regexp-in-string ":" "")
                      (replace-regexp-in-string "T" " " )
                      (replace-regexp-in-string "+" " +")))))
    (--map (or it 0) parsed)))

(defun kubernetes-time-diff-string (start now)
  "Find the interval between START and NOW, and return a string of the coarsest unit."
  (let ((diff (time-to-seconds (time-subtract now start))))
    (car (split-string (format-seconds "%yy,%dd,%hh,%mm,%ss%z" diff) ","))))

(defun kubernetes-pod-label (pod)
  (-let [(&alist 'metadata (&alist 'labels (&alist "name" name "job-name" job-name))) pod]
    (or name job-name)))

(defun kubernetes-pods-for-label (label-name state)
  (let ((results (kubernetes-state-pods (kubernetes-state-empty)))
        (pods (kubernetes-state-pods state)))
    (dolist (key (hash-table-keys pods))
      (-when-let* ((pod (gethash key pods))
                   (name (kubernetes-pod-label pod)))
        (when (equal name label-name)
          (puthash key pod results))))
    results))

(defun kubernetes-sorted-keys (ht)
  (-sort (lambda (l r) (string< (symbol-name l) (symbol-name r)))
         (hash-table-keys ht)))

(defun kubernetes-read-label ()
  (let ((labels (-sort #'string< (-keep #'kubernetes-pod-label (hash-table-values (kubernetes-state-pods))))))
    (completing-read "Label: " labels nil t nil 'kubernetes-labels)))

(provide 'kubernetes-utils)

;;; kubernetes-utils.el ends here
