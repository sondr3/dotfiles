;;; +shell.el --- Amalthea support for shell scripting -*- lexical-binding: t -*-

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

;; Support for writing shell scripts in Bash, Fish and more. Nothing out of the
;; ordinary here folks.

;;; Code:

;;; `fish-mode':
;; Because Fish is the superior shell.
(use-package fish-mode
  :init (csetq fish-indent-offset amalthea-tab-width))

;;; `sh-script':
;; Make sure we have the `shellcheck' package for linting of shell files and add
;; a bunch of key bindings to `sh-mode' for easier access, these are essentially
;; the same keys that are under `C-c'.
(use-package sh-script
  :ghook ('sh-mode-hook (list #'subword-mode #'flycheck-mode)))

;;; `company-shell':
;; Adds auto completion for shell scripting to Company.
(use-package company-shell
  :after company
  :init (add-to-list 'company-backends '(company-shell company-shell-env company-fish-shell)))

(provide '+shell)
;;; +shell.el ends here
