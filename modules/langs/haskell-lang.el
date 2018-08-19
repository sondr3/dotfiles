;;; haskell-lang.el --- Haskell support -*- lexical-binding: t -*-

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

(use-package haskell-mode
  :ensure-system-package (brittany . "stack install brittany")
  :delight (subword-mode)
  :ghook ('haskell-mode-hook (list #'interactive-haskell-mode #'subword-mode #'haskell-auto-insert-module-template))
  :init
  (add-to-list 'recentf-exclude (expand-file-name "~/.stack/global-project/.stack-work/")) ;; Exclude Intero REPL from recentf
  :config
  (setq haskell-compile-cabal-build-command "stack build --fast" ;; We're using Stack instead of Cabal due to Intero
        haskell-process-type 'stack-ghci                         ;; Always use Stack with GHCi
        haskell-process-suggest-remove-import-lines t            ;; Suggest removing imports
        haskell-process-auto-import-loaded-modules t             ;; Automatically load modules
        haskell-interactive-popup-errors nil                     ;; Unnecessary because of Flycheck
        haskell-process-show-overlays nil))                      ;; Same as above

(use-package intero
  :after haskell-mode
  :commands intero-global-mode
  :delight " λ"
  :init (intero-global-mode))

(use-package flycheck-haskell
  :after (intero flycheck)
  :commands (flycheck-haskell-configure flycheck-add-next-checker)
  :ghook ('flycheck-mode-hook #'flycheck-haskell-configure)
  :init (flycheck-add-next-checker 'intero '(warning . haskell-hlint)))

(use-package hlint-refactor
  :ensure-system-package
  ((hlint . "stack install hlint")
   (refactor . "stack --resolver=nightly install apply-refact"))
  :general
  (amalthea-major-leader 'haskell-mode-map
    "r" '(:ignore t :wk "refactor")
    "r b" '(hlint-refactor-refactor-buffer :wk "refactor buffer")
    "r r" '(hlint-refactor-refactor-at-point :wk "refactor at point")))

(provide 'haskell-lang)
;;; haskell-lang.el ends here
