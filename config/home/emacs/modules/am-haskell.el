;;; am-haskell.el --- Amalthea Haskell configuration -*- lexical-binding: t -*-

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

;;; Code:
;;; `haskell-mode':
;; The main focal point of the Haskell editing experience, there's no magic
;; here. All it does is add some modes to `haskell-mode', exclude some project
;; files from `recentf' and set a few common sense settings.
(use-package haskell-mode
  :ghook ('haskell-mode-hook (list #'subword-mode
                                   #'haskell-auto-insert-module-template
                                   #'haskell-collapse-mode
                                   #'interactive-haskell-mode
                                   #'direnv-update-environment))
  :ghook ('haskell-interactive-mode-hook (list #'evil-insert-state
                                               #'add-pragmatapro-prettify-symbols-alist
                                               #'setup-compose-predicate))
  :general
  (amalthea-major-leader 'haskell-mode-map
    "f" '(haskell-mode-stylish-buffer :wk "format buffer")
    "F" '(haskell-mode-format-imports :wk "format imports"))
  :init (require 'lsp-haskell)
  :config
  (progn
    (csetq haskell-process-type 'cabal-new-repl          ;; Use the new Cabal REPL
           haskell-mode-stylish-haskell-path "brittany"  ;; Format files with Brittany instead of Stylish
           haskell-stylish-on-save t                     ;; Format buffer with Brittany on save
           haskell-process-suggest-remove-import-lines t ;; Suggest removing imports
           haskell-process-auto-import-loaded-modules t  ;; Automatically load modules
           haskell-interactive-popup-errors nil          ;; Unnecessary because of Flycheck
           haskell-process-show-overlays nil)))          ;; Same as above

;;; `lsp-haskell':
;; Intero has been sunset and as such we migrate to the next big thing; language
;; servers. We're using HIE (Haskell IDE Engine) with LSP.
(use-package lsp-haskell
  :after haskell-mode
  :ghook ('haskell-mode-hook #'lsp))

;;; `flycheck-haskell':
;; We obviously need some kind of error correction, for this we'll use `hlint',
;; which is a linter for Haskell code. We need to manually add this as a warning
;; to Flycheck, but this is done after Flycheck has loaded.
(use-package flycheck-haskell
  :after flycheck
  :commands (flycheck-haskell-setup)
  :ghook ('flycheck-mode-hook #'flycheck-haskell-setup))

;;; `hlint-refactor':
;; A lot of the time `hlint' can also apply fixes to our code for us, this is
;; done via this package. We install the required dependencies and add a few
;; keybindings for it.
(use-package hlint-refactor
  :general
  (amalthea-major-leader 'haskell-mode-map
    "r" '(:ignore t :wk "refactor")
    "r b" '(hlint-refactor-refactor-buffer :wk "refactor buffer")
    "r r" '(hlint-refactor-refactor-at-point :wk "refactor at point")))

;; Rename and hide some modes
(delight '((haskell-mode "" :major)
           (interactive-haskell-mode nil "haskell")
           (haskell-collapse-mode nil "haskell-collapse")
           (subword-mode nil "subword")))

(provide 'am-haskell)

;;; am-haskell.el ends here
