;;; org-agenda.el --- Configuration for my life -*- lexical-binding: t -*-

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

(use-package org-agenda
  :after org
  :init (add-to-list 'org-modules 'org-habit t)
  :config
  (progn
    (csetq org-agenda-files '("~/.org/routine.org"
                              "~/.org/school.org"
                              "~/.org/work.org"
                              "~/.org/workouts.org"))))

(use-package org-super-agenda
  :after org)

(provide 'ox-agenda)
;;; org-agenda.el ends here