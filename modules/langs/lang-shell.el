;;; lang-shell.el --- Amalthea support for shell scripting -*- lexical-binding: t -*-

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

;;; `sh-script':
;; Make sure we have the `shellcheck' package for linting of shell files and add
;; a bunch of key bindings to `sh-mode' for easier access, these are essentially
;; the same keys that are under `C-c'.
(use-package sh-script
  :ensure-system-package shellcheck
  :ghook ('sh-mode-hook (list #'subword-mode #'flycheck-mode))
  :init (add-hook 'sh-mode-hook #'flycheck-mode)
  :general
  (amalthea-major-leader 'sh-mode-map
    "a" '(sh-add :wk "add")
    "i" '(:ignore t :wk "insert")
    "i c" '(sh-case :wk "case")
    "i i" '(sh-if :wk "if")
    "i f" '(sh-function :wk "function")
    "i o" '(sh-for :wk "for")
    "i e" '(sh-indexed-loop :wk "indexed for")
    "i w" '(sh-while :wk "while")
    "i r" '(sh-repeat :wk "repeat")
    "i s" '(sh-select :wk "select")
    "i u" '(sh-until :wk "until")
    "i g" '(sh-while-getopts :wk "getopts")
    "s" '(:ignore t :wk "shell")
    "s d" '(sh-cd-here :wk "open shell in $DIR")
    "s s" '(sh-set-shell :wk "set shell")
    "s z" '(sh-show-shell :wk "show shell")
    "e" '(executable-interpret :wk "eval")
    "t" '(sh-tmp-file :wk "tmp")
    "\\" '(sh-backslash-region :wk "comment")))

;;; `company-shell':
;; Adds auto completion for shell scripting to Company.
(use-package company-shell
  :after company
  :init (add-to-list 'company-backends '(company-shell company-shell-env company-fish-shell)))

;;; `flycheck-bashate':
;; Bashate is essentially PEP8 for Bash scripts, so we'll add that to Flycheck
;; to get error messages.
(use-package flycheck-bashate
  :ensure-system-package (bashate . "pip3 install bashate")
  :after flycheck
  :commands flycheck-bashate-setup
  :init (flycheck-bashate-setup))

;;; `flycheck-checkbashisms':
;; `checkbashisms' is a package that is used by Debian developers to ensure that
;; they don't to silly things while writing shell scripts, so we'll enable that
;; as well for our shell scripts.
(use-package flycheck-checkbashisms
  :ensure-system-package checkbashisms
  :after flycheck
  :commands flycheck-checkbashisms-setup
  :ghook ('sh-mode-hook #'flycheck-checkbashisms-setup)
  :config
  (progn
    (setq flycheck-checkbashisms-newline t
          flycheck-checkbashisms-posix t)))

(provide 'lang-shell)
;;; lang-shell.el ends here
