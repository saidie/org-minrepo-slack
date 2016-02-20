;;; org-minrepo-slack.el --- Report current clocking-in task to Slack

;; Copyright (C) 2016 by Hiroshi Saito

;; Author: Hiroshi Saito <monodie@gmail.com>
;; URL: https://github.com/saidie/org-minrepo-slack
;; Version: 0.9.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'json)

(defvar oms/slack-webhook-url nil)
(defvar oms/slack-options nil)

(defvar oms/text-function
  (lambda () (format "Started `%s`" org-clock-heading)))

(defvar oms/task-select-function
  (lambda ()
    (let ((tags (nth 5 (org-heading-components))))
      (and tags (string-match ":@OFFICE:" tags)))))

(defvar oms/notification-delay "3 min")

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
               (funcall oms/task-select-function))
      (message "will notify to Slack")
      (run-at-time oms/notification-delay nil
                   'oms/send-notification current-task))))

(provide 'org-minrepo-slack)

;;; org-minrepo-slack.el ends here
