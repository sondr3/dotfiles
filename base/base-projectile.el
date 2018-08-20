;;; base-projectile.el --- Configuration for Projectile -*- lexical-binding: t -*-

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

(use-package projectile
  :commands projectile-mode
  :delight " Ⓟ"
  :init
  (progn
    (setq projectile-completion-system 'ivy
          projectile-sort-order 'recentf
          projectile-enable-caching t
          projectile-cache-file (concat amalthea-cache-dir "projectile.cache")
          projectile-known-projects-file (concat amalthea-cache-dir "projectile-bookmarks.eld"))
    (projectile-mode)))

(use-package counsel-projectile
  :after projectile
  :commands counsel-projectile-mode
  :general
  (amalthea-leader
    "p" '(:ignore t :wk "project"))
  :init (counsel-projectile-mode))

(provide 'base-projectile)
;;; base-projectile.el ends here
