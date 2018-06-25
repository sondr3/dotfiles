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

;; commentary

;;; Code:

(use-package company
  :delight " Ⓒ"
  :hook (prog-mode . company-mode)
  :general
  (:keymaps 'company-mode-map :states 'insert
            [tab] 'company-complete)
  :init
  (progn
    (setq company-idle-delay 0.2
          company-tooltip-limit 20
          company-show-numbers t
          company-tooltip-align-annotations t
          company-require-match nil
          company-dabbrev-ignore-case nil
          company-dabbrev-downcase nil
          company-dabbrev-other-buffers t)))

(use-package company-box
  :after company
  :delight
  :ghook 'company-mode-hook
  :custom
  (company-box-backends-colors nil "Don't use colors for the various backends"))

(use-package company-quickhelp
  :after company
  :commands company-quickhelp-mode
  :custom
  (company-quickhelp-use-propertized-text t "Allow text to have properties like size, color etc")
  :config (company-quickhelp-mode))

(use-package company-statistics
  :after company
  :ghook 'company-mode-hook
  :custom
  (company-statistics-file (concat amalthea-cache-dir "company-statistics.el") "Location to save statistics"))

(provide 'base-completion)
;;; base-completion.el ends here
