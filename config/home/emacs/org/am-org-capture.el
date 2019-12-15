;;; am-org-capture.el --- Amalthea Org-mode capture configuration -*- lexical-binding: t -*-

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:
;; Capturing of todos and fixmes and so on in projects and in regular life. Uses
;; `counsel-projectile-org-capture' to automatically put them in their correct
;; projects and if not puts them in my inbox.

;;; Code:
(defcustom amalthea-capture-projects "~/Dropbox/org/projects.org"
  "Inbox file for project related tasks."
  :type 'string
  :group 'amalthea)

(defcustom amalthea-capture-inbox "~/Dropbox/org/inbox.org"
  "Inbox for personal tasks, reminders and so on."
  :type 'string
  :group 'amalthea)

(defcustom amalthea-capture-notes "~/Dropbox/org/notes.org"
  "Location of notes file for assorted things that I want to remark on."
  :type 'string
  :group 'amalthea)

(defcustom amalthea-capture-school "~/Dropbox/org/school.org"
  "Location of school file for reminders about subjects, volunteering etc."
  :type 'string
  :group 'amalthea)

(use-package org-capture
  :ghook ('org-capture-mode-hook #'evil-insert-state)
  :general
  (amalthea-leader "c" '(counsel-projectile-org-capture :wk "capture"))
  :init
  (progn
    (csetq org-capture-templates '(("t" "Personal TODO" entry
                                    (file+headline amalthea-capture-inbox "Inbox")
                                    "* TODO %?\n  %u\n  %a")
                                   ("n" "Personal note" entry
                                    (file+headline amalthea-capture-notes "Inbox")
                                    "* %?\n  %u\n  %a")
                                   ("s" "School TODO" entry
                                    (file+headline amalthea-capture-school "Inbox")
                                    "* TODO %?"))
           counsel-projectile-org-capture-templates '(("pt" "[${name}] TODO" entry
                                                       (file+headline amalthea-capture-projects "${name}")
                                                       "* TODO %? %u\n")
                                                      ("pf" "[${name}] FIXME" entry
                                                       (file+headline amalthea-capture-projects "${name}")
                                                       "* FIXME %? %t\n")))))

(provide 'am-org-capture)

;;; am-org-capture.el ends here
