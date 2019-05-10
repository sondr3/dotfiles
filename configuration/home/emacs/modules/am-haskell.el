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
;; Configuration for the Haskell language, this package requires you to have
;; `stack' installed, as `intero' uses it.

;;; Code:
;;; `haskell-mode':
;; The main focal point of the Haskell editing experience, there's no magic
;; here. All it does is add some modes to `haskell-mode', exclude some project
;; files from `recentf' and set a few common sense settings.
(use-package haskell-mode
  :ghook ('haskell-mode-hook (list #'subword-mode #'haskell-auto-insert-module-template))
  :general
  (amalthea-major-leader 'haskell-mode-map
    "f" '(haskell-mode-stylish-buffer :wk "format buffer")
    "F" '(haskell-mode-format-imports :wk "format imports"))
  :init
  (add-to-list 'recentf-exclude (expand-file-name "~/.stack/global-project/.stack-work/")) ;; Exclude Intero REPL from recentf
  :config
  (csetq haskell-compile-cabal-build-command "stack build --fast" ;; We're using Stack instead of Cabal due to Intero
         haskell-process-type 'stack-ghci                         ;; Always use Stack with GHCi
         haskell-mode-stylish-haskell-path "brittany"             ;; Format files with Brittany instead of Stylish
         haskell-stylish-on-save t                                ;; Format buffer with Brittany on save
         haskell-process-suggest-remove-import-lines t            ;; Suggest removing imports
         haskell-process-auto-import-loaded-modules t             ;; Automatically load modules
         haskell-interactive-popup-errors nil                     ;; Unnecessary because of Flycheck
         haskell-process-show-overlays nil))                      ;; Same as above

;;; `intero':
;; The main workhorse for working with Haskell, Intero is both a Haskell program
;; and a Emacs mode. It gives you a way to load your code into the REPL, work
;; inside the REPL, send code back and so on. It's similar to SLIME for Common
;; Lisp.
(use-package intero
  :after haskell-mode
  :commands intero-global-mode
  :delight "λ"
  :general
  (amalthea-major-leader 'haskell-mode-map
    "." '(intero-goto-definition :wk "goto definition")
    "?" '(intero-uses-at :wk "show usage")
    "t" '(intero-type-at :wk "type info")
    "i" '(intero-info :wk "info")
    "l" '(intero-repl-load :wk "load into REPL")
    "e" '(intero-repl-eval-region :wk "eval region")
    "E" '(intero-expand-splice-at-point :wk "expand splice")
    "a" '(intero-apply-suggestions :wk "apply suggestions")
    "s" '(intero-repl :wk "switch to REPL")
    "h" '(hoogle :wk "hoogle")
    "H" '(hayoo :wk "hayoo"))
  (amalthea-major-leader 'intero-repl-mode-map
    "s" '(intero-repl-switch-back :wk "switch back")
    "l" '(intero-repl-clear-buffer :wk "clear REPL"))
  :init (intero-global-mode))

;;; `flycheck-haskell':
;; We obviously need some kind of error correction, for this we'll use `hlint',
;; which is a linter for Haskell code. We need to manually add this as a warning
;; to Flycheck, but this is done after both Intero and Flycheck has loaded.
(use-package flycheck-haskell
  :after (intero flycheck)
  :commands (flycheck-haskell-configure flycheck-add-next-checker)
  :ghook ('flycheck-mode-hook #'flycheck-haskell-configure)
  :init (flycheck-add-next-checker 'intero '(warning . haskell-hlint)))

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

(delight '((haskell-mode "" :major)))

(provide 'am-haskell)

;;; am-haskell.el ends here
