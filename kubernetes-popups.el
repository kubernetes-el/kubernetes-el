;;; kubernetes-popups.el --- Magit popups for Kubernetes buffers.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'magit-popup)

(defgroup kubernetes nil
  "Emacs porcelain for Kubernetes."
  :group 'tools
  :prefix "kubernetes-")

(magit-define-popup kubernetes-logs-popup
  "Popup console for logging commands for POD."
  :group 'kubernetes
  :options
  '("Options for customizing logging behaviour"
    (?t "Number of lines to display" "--tail=" read-number "-1")
    "Time controls"
    (?s "Since relative time" "--since=" kubernetes-utils-read-time-value)
    (?d "Since absolute datetime" "--since-time=" kubernetes-utils-read-iso-datetime))
  :switches
  '((?p "Print logs for previous instances of the container in this pod" "-p"))
  :actions
  '((?l "Logs" kubernetes-logs-fetch-all)
    (?f "Logs (stream and follow)" kubernetes-logs-follow))
  :max-action-columns 2
  :default-action 'kubernetes-logs)

(magit-define-popup kubernetes-overview-popup
  "Popup console for showing an overview of available popup commands."
  :group 'kubernetes
  :actions
  '("Environment"
    (?c "Configuration" kubernetes-config-popup)
    "Marking pods"
    (?D "Delete pod at point" kubernetes-mark-for-delete)
    (?u "Unmark pod at point" kubernetes-unmark)
    (?U "Unmark all pods" kubernetes-unmark-all)
    "Popup commands"
    (?d "Describe" kubernetes-describe-popup)
    (?e "Exec" kubernetes-exec-popup)
    (?l "Logs" kubernetes-logs-popup)
    "Misc"
    (?h "Describe mode and keybindings" describe-mode)))

(magit-define-popup kubernetes-config-popup
  "Popup console for showing an overview of available config commands."
  :group 'kubernetes
  :actions
  '("Managing contexts"
    (?c "Change context" kubernetes-use-context)
    "Query settings"
    (?n "Set namespace" kubernetes-set-namespace)))

(magit-define-popup kubernetes-exec-popup
  "Popup console for exec commands for POD."
  :group 'kubernetes
  :default-arguments '("-i" "-t")
  :switches
  '((?i "Pass stdin to container" "-i" t)
    (?t "Stdin is a TTY" "-t" t))
  :actions
  '((?e "Exec" kubernetes-exec-into))

  :default-action 'kubernetes-exec-into)

(magit-define-popup kubernetes-describe-popup
  "Popup console for describe commands."
  :group 'kubernetes
  :actions
  '((?d "Dwim" kubernetes-describe-dwim)
    (?p "Pod" kubernetes-describe-pod))
  :default-action 'kubernetes-logs)


(provide 'kubernetes-popups)

;;; kubernetes-popups.el ends here
