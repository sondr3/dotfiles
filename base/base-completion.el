;;; base-completion.el --- Auto-completion -*- lexical-binding: t -*-

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

;; Provides auto completion powered by `Company', snippet expansions powered by
;; `Yasnippet' and `hippie-expand'.

;;; Code:

;;; `Company':
;; Instead of using something like `auto-complete' we'll use `Company' to give
;; us auto completion for variables, functions and so on.
(use-package company
  :delight " Ⓒ"
  :hook (prog-mode . company-mode)
  :general
  (:keymaps 'company-mode-map :states 'insert
            [tab] 'company-complete
            "C-n" 'company-select-next
            "C-p" 'company-select-previous)
  :custom
  (company-idle-delay 0.2 "How long to wait before popping up")
  (company-tooltip-limit 20 "Limit on how many options to display")
  (company-show-numbers t "Show numbers behind options")
  (company-tooltip-align-annotations t "Align annotations to the right")
  (company-require-match nil "Allow free typing")
  (company-dabbrev-ignore-case nil "Don't ignore case when completing")
  (company-dabbrev-downcase nil "Don't automatically downcase competions")
  (company-dabbrev-other-buffers t "Search other buffers for completion candidates"))

;;; `company-box':
;; Instead of using the default tooltip box that `Company' comes with we'll
;; instead shell out to a different package that gives us a really nice... box.
;; It allows for icons next to completion canditates as well as different colors
;; for various backends if the need is there.
(use-package company-box
  :after company
  :delight
  :ghook 'company-mode-hook
  :custom
  (company-box-backends-colors nil "Don't use colors for the various backends"))

;;; `company-quickhelp':
;; When idling on a chosen completion candidate, show the items help in a popup
;; box next to the completion window.
(use-package company-quickhelp
  :after company
  :commands company-quickhelp-mode
  :custom
  (company-quickhelp-use-propertized-text t "Allow text to have properties like size, color etc")
  :config (company-quickhelp-mode))

;;; `company-statistics':
;; When completing a candidate, save the candidate to a history file and sort
;; completions accordingly next time so the candidate is ranked higher than the
;; last time. Useful for when there are many options but you mostly select one
;; or a few of them.
(use-package company-statistics
  :after company
  :ghook 'company-mode-hook
  :custom
  (company-statistics-file (concat amalthea-cache-dir "company-statistics.el") "Location to save statistics"))

(provide 'base-completion)
;;; base-completion.el ends here
