;;; am-org-agenda.el --- Amalthea Org-mode agenda configuration -*- lexical-binding: t -*-

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

;; This is my life, yes.

;;; Code:

;;; Agenda
(use-package org-agenda
  :after org
  :init (add-to-list 'org-modules 'org-habit t)
  :config
  (progn
    (csetq org-agenda-files '("~/Dropbox/org/routine.org"
                              "~/Dropbox/org/school.org"
                              "~/Dropbox/org/work.org"))))

(use-package org-super-agenda
  :after org)

(provide 'am-org-agenda)

;;; am-agenda.el ends here
