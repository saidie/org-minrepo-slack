(require 'json)

(defvar oms/slack-webhook-url nil)
(defvar oms/slack-options nil)

(defvar oms/text-function
  (lambda () (format "Started `%s`" org-clock-heading)))

(defvar oms/task-check-function
  (lambda ()
    (let ((tags (nth 5 (org-heading-components))))
      (string-match ":@OFFICE:" tags))))

(defvar oms/notification-buffer "3 min")

(defun oms/send-to-slack (url text &optional options)
  (let ((payload (json-encode (plist-put (or options '()) :text text))))
    (call-process "curl" nil 0 nil "-X" "POST"
                  "-H" "Content-Type: application/json" "--data" payload url)))

(defun oms/send-notification (old-task)
  (when (and (org-clocking-p)
             (eq old-task org-clock-current-task))
    (oms/send-to-slack oms/slack-webhook-url
                       (funcall oms/text-function)
                       oms/slack-options)))

(defun oms/register-notification ()
  (let ((current-task org-clock-current-task))
    (when (and (stringp oms/slack-webhook-url)
               (funcall oms/task-check-function))
      (message "will notify to Slack")
      (run-at-time oms/notification-buffer nil
                   'oms/send-notification current-task))))
