;;; am-editor.el --- Amalthea editor configuration -*- lexical-binding: t -*-

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
;; Contains configuration and settings for packages that are what I'd consider
;; core to a proper usage of Emacs.

;;; Code:
;;; Line wrapping:
;; Following the above, we'll use 80 as the default width for which to wrap text
;; in all modes, because that's modern. At least more modern than the default
;; for Emacs, which is a prehistoric 70.
(defvar amalthea-fill-width 80
  "The default width at which to wrap text in Amalthea.")

;;; Indentation:
;; Emacs defaults to both using tabs for indentation and the width for a tab
;; character is a whopping eight! That doesn't make any sense whatsoever. So
;; we'll make it sane, use spaces and with a width of two.
(defvar amalthea-tab-width 2
  "The default width for indentation, in spaces, in Amalthea.")

(setq-default indent-tabs-mode nil                   ;; Don't use tabs, use spaces
              tab-width amalthea-tab-width           ;; Set our tab width
              fill-column amalthea-fill-width        ;; Automatically wrap lines after this point
              compilation-scroll-output 'first-error ;; Stop at the first error in compilation log
              word-wrap t                            ;; Wrap long lines instead of sending them outside the scree
              require-final-newline t)               ;; Always end files with a newline

;; Enable `auto-fill-mode' for any and all `text-mode' major modes to enable
;; wrapping of text at whatever width we enabled `amalthea-fill-width' to be.
(add-hook 'text-mode-hook #'auto-fill-mode)
(delight 'auto-fill-function nil t)

;;; Help:
;; Emacs has amazing documentation and builtin help pages and functions and
;; everything, it explains more or less anything that you need to know.
;; Functions, variables, modes, how to say hello in 100+ languages and so on.
;; There's not a lot of configuration that we'll do here, but we'll do a few
;; keybindings of our own.
(use-package help
  :commands temp-buffer-resize-mode
  :init (temp-buffer-resize-mode)
  :config (csetq help-window-select t)) ;; Automatically go to help window
;;; `helpful':
;; The name basically tells you what it does, it makes the help buffer show a
;; lot more information. We bind the `helpful' functions that have no
;; counterpart in Emacs and remap those that do.
(use-package helpful
  :general
  (amalthea-leader
    "h c" '(helpful-command :wk "describe command")
    "h d" '(helpful-at-point :wk "describe at point")
    "h f" '(describe-function :wk "describe function")
    "h F" '(helpful-function :wk "helpful function")
    "h k" '(describe-key :wk "describe key")
    "h m" '(helpful-macro :wk "describe macro")
    "h M" '(describe-mode :wk "describe mode")
    "h v" '(describe-variable :wk "describe variable"))
  (:keymaps 'override
            [remap describe-function] 'helpful-callable
            [remap describe-key] 'helpful-key
            [remap describe-variable] 'helpful-variable))

;;; `paren':
;; Does pretty much exactly what it says, it shows matching parenthesizes (and
;; other delimiters as far as I'm aware too). As for settings, we'll set it so
;; there's no delay for showing it's long lost sister, always highlight open
;; parenthesises and show the matching pair when inside their block.
(use-package paren
  :commands (show-paren-mode)
  :init (show-paren-mode)
  :config
  (progn
    (setq-default show-paren-delay 0                      ;; Show matching parenthesis without delay.
                  show-paren-highlight-openparen t        ;; Always show the matching parenthesis.
                  show-paren-when-point-inside-paren t))) ;; Show parenthesis when inside a block.

;;; `autorevert':
;; If you've ever experienced changing a file in a different program while it's
;; open in Emacs (for whatever reason) and then mistakenly overwriting it again
;; when you save it in Emacs because it hasn't been refreshed from disk? Worry
;; no more. As for configuration, the only thing we'll change is that it doesn't
;; just refresh file buffers, but also buffers that indirectly have to do with
;; files, e.g. Dired buffers and such.
(use-package autorevert
  :commands (global-auto-revert-mode)
  :init
  (progn
    (csetq global-auto-revert-non-file-buffers t ;; Refresh any buffer that implement autorevert
           auto-revert-verbose nil)              ;; Be silent when refreshing a buffer
    (global-auto-revert-mode)))

;;; `recentf':
;; Intead of having to work your way to the most recently edited file(s) by
;; writing the path out again and again, Emacs has a built-in minor mode that
;; keeps track of the most recently visited files, which we'll use in
;; conjunction with Counsel to quickly be able to open recent files. The way we
;; load it is stolen from Spacemacs, which makes it so it's lazily loaded when
;; needed.
(use-package recentf
  :commands (recentf-mode recentf-track-opened-file)
  :init
  (progn
    (csetq recentf-max-saved-items 1000                            ;; Total amount of saved recent files
           recentf-auto-cleanup 'never)                            ;;  Never clean the history, only append and remove the last
    (recentf-mode))
  :config
  (progn
    (add-to-list 'recentf-exclude no-littering-var-directory)   ;; Don't put litter in `recentf'
    (add-to-list 'recentf-exclude no-littering-etc-directory))) ;; Don't put litter in `recentf'

;;; `savehist':
;; This is probably one of the easier minor modes to explain, so we'll keep it
;; brief: it saves a history of everything you do in a minibuffer.
(use-package savehist
  :commands (savehist-mode)
  :init
  (progn
    (csetq savehist-save-minibuffer-history t                         ;; Save history from minibuffer too
           history-length 1000                                        ;; Total amount of history to save
           savehist-autosave-interval 60                              ;; Save every minute
           savehist-additional-variables '(mark-ring                  ;; Additional variables to save
                                           global-mark-ring
                                           search-ring
                                           regexp-search-ring
                                           extended-command-history))
    (savehist-mode)))

;;; `saveplace':
;; Mostly the same as above, instead of keeping track of the history of what you
;; did in your minibuffers, it keeps track of where the cursor was last in a
;; file and saves that position so that when you reopen that file you'll start
;; at the same place as you left.
(use-package saveplace
  :commands (save-place-mode)
  :init (save-place-mode))

;;; `uniquify':
;; Whenever you have multiple files with the same name open, you need a way to
;; differentiate between the two of them. We'll make it so that two files with
;; the same name, it shows the full path instead of the default, which I quite
;; frankly don't remember.
(use-package uniquify
  :init
  (csetq uniquify-buffer-name-style 'forward)) ;; How to name multiple buffers with the same name

;;; `undo-tree':
;; This is essentially the undo command on steroids, it creates a tree of
;; changes that you can revert back and from with, meaning you can undo
;; something, change your mind, go back to the parent node and start from there
;; and then go back to the previous "branch" again if you change your mind...
;; again
(use-package undo-tree
  :commands global-undo-tree-mode
  :delight
  :init (global-undo-tree-mode))

;;; `rainbow-delimiters':
;; This is fairly straight forward, it matches pairs of parens with colors,
;; making it easier to at a glance see blocks of code.
(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :ghook ('prog-mode-hook #'rainbow-delimiters-mode))

;;; `aggressive-indent':
;; The default indentation mode for Emacs is okay, but when editing LISP you can
;; do so much more. Since it's not whitespace sensitive you're free to
;; manipulate it at will with packages like `smartparens' or `lispy'. This minor
;; mode aggressively indents code whenever you change any part of a code block.
(use-package aggressive-indent
  :delight
  :ghook ('emacs-lisp-mode-hook #'aggressive-indent-mode))

;;; `ws-butler':
;; This is something that you could fix by using a builtin helper function that
;; removes newlines at the end of files etc, but I prefer using this package
;; which is way more thorough.
(use-package ws-butler
  :delight
  :commands ws-butler-global-mode
  :init (ws-butler-global-mode))

;;; `deadgrep':
;; Gives you super powered searching via `ripgrep' and `deadgrep'. Sweet, sweet
;; searching.
(use-package deadgrep
  :general
  (amalthea-leader
    "a s" '(deadgrep :wk "ripgrep")))

;;; Shell
(defun vterm-init ()
  "Initialize vterm properly."
  (interactive "P")
  (toggle-truncate-lines t)
  (visual-line-mode 0))

(use-package vterm
  :commands (vterm)
  :ghook ('vterm-mode-hook (lambda ()
                             (toggle-truncate-lines t))))

(provide 'am-editor)

;;; am-editor.el ends here
