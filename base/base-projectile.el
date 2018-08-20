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

;; Configures Projectile for some sweet, sweet project awesomeness.

;;; Code:

;;; `projectile':
;; Projectile is a program for working with projects in Emacs, it supports a ton
;; of features out of the box that are awesome and useful, like searching for
;; files only in the current project, recent files in current project and so on.
(use-package projectile
  :commands projectile-mode
  :delight " Ⓟ"
  :init
  (progn
    (setq projectile-completion-system 'ivy                                                      ;; Use Ivy for completion
          projectile-sort-order 'recentf                                                         ;; Sort by using `recentf'
          projectile-enable-caching t                                                            ;; Enable caching to speed up searching, finding files
          projectile-cache-file (concat amalthea-cache-dir "projectile.cache")                   ;; Save cache file in cache dir
          projectile-known-projects-file (concat amalthea-cache-dir "projectile-bookmarks.eld")) ;; Save projects in cache dir
    (projectile-mode)))

;;; `counsel-projectile':
;; Even though we've configured Projectile to use Ivy, we can extend it even
;; more by also using Counsel too.
(use-package counsel-projectile
  :after projectile
  :commands counsel-projectile-mode
  :general
  (amalthea-leader
    "p" '(projectile-command-map :wk "project"))
  :init (counsel-projectile-mode))

(provide 'base-projectile)
;;; base-projectile.el ends here
