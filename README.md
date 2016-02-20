# org-minrepo-slack

## About

This package helps to send a minutely report by automatically posting your clocking-in task.

## Requirement

- cURL

## Usage

Put the following lines in your `.emacs` file.

```lisp
(require 'org-minrepo-slack)
(add-hook 'org-clock-in-hook 'oms/register-notification)

;; Set your Webhook URL
(setq oms/slack-webhook-url "https://hooks.slack.com/services/XXXXXXXXXXXXXXXX")
```

By default, a notification will be posted to Slack after 3 minutes when you clocked-in to a task with `@OFFICE` tag. Note that a notification is not sent when you left the task within 3 minutes.

Several variables are provided to customize behavior:

```lisp
;; A delay of notification after clock-in
(defvar oms/notification-delay "5 min")

;; Optional Slack Webhook parameters
(setq oms/slack-options
      '(:channel "#test" :username "emacs" :icon_emoji ":emacs:"))

;; A function to format a text (default)
(defvar oms/text-function
  (lambda () (format "Started `%s`" org-clock-heading)))

;; A function to select a notified task (default)
(defvar oms/task-select-function
  (lambda ()
    (let ((tags (nth 5 (org-heading-components))))
      (and tags (string-match ":@OFFICE:" tags)))))
```
