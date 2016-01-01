;;; -*- lexical-binding: t -*-
;;; ohai-orgmode-custom.el

;; Copyright (C) 2015 Shafayet Khan

;; Author: Shafayet Khan

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

;; Configure org-capture for todos and notes

(setq org-default-notes-file "~/Org/notes.org")

(global-set-key (kbd "C-c c") 'org-capture)

(setq org-capture-templates
      `(("t" "todo")
        ("tw" "work" entry (file+headline "~/Org/gtd.org" "Inbox")
         "* JIRA %?\n   SCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+1d\"))\n%U\n" :clock-resume t :prepend t)
        ("tt" "task" entry (file+headline "~/Org/gtd.org" "Inbox")
         "* NEXT %?\n   SCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+1d\"))\n%U\n" :clock-resume t :prepend t)
        ("n" "note" entry (file+headline "" "Bank") ; "" => org-default-notes-file
         "* %? :@note:\n%U\n%a\n" :clock-resume t :prepend t)
        ("m" "meeting" entry (file+headline "~/Org/gtd.org" "Meetings")
         "* MEETING with %? :@meeting:\n%U" :clock-in t :clock-resume t :prepend t)
        ))

;; Temporarily require "request-deferred" for org-todoist
;; Send requests and return deferred object associated with it.
(package-require 'request-deferred)

;; Temporary require org-todoist until package is in melpa
(when (require 'org-todoist nil t)
  (message "ORG-TODOIST prototype has been loaded for testing."))

(provide 'ohai-orgmode-custom)
;;; ohai-orgmode-custom.el ends here
