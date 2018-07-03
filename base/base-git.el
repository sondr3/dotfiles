;;; base-git.el --- Git -*- lexical-binding: t -*-

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

;; commentary

;;; Code:

(use-package magit
  :delight auto-revert-mode
  :general
  (amalthea-leader
    :keymaps 'normal
    "g" '(:ignore t :which-key "git")
    "g s" '(magit-status :which-key "git status"))
  :config
  (progn
    (magit-add-section-hook 'magit-status-sections-hook
                            'magit-insert-modules
                            'magit-insert-stashes
                            'append)))

(use-package evil-magit
  :after magit
  :commands evil-magit-init
  :init (evil-magit-init))

(provide 'base-git)

;;; base-git.el ends here
