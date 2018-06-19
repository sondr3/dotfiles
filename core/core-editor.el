;;; core-editor.el --- Core editor settings -*- lexical-binding: t -*-

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

;; Indentation:

;; Emacs defaults to both using tabs for indentation and the width for a tab
;; character is a whopping eight! That doesn't make any sense whatsoever. So
;; we'll make it sane, use spaces and with a width of two.

;; Line wrapping:

;; Following the above, we'll use 80 as the default width for which to wrap text
;; in all modes, because that's modern. At least more modern than the default
;; for Emacs, which is a prehistoric 70.

;; Help:

;; Emacs has amazing documentation and builtin help pages and functions and
;; everything, it explains more or less anything that you need to know.
;; Functions, variables, modes, how to say hello in 100+ languages and so on.
;; There's not a lot of configuration that we'll do here, but we'll do a few
;; keybindings of our own.

;; `paren':

;; Does pretty much exactly what it says, it shows matching parenthesises (and
;; other delimiters as far as I'm aware too). As for settings, we'll set it so
;; there's no delay for showing it's long lost sister, always highlight open
;; parenthesises and show the matching pair when inside their block.

;;; Code:

(defcustom amalthea-fill-width 80
  "The default width at which to wrap text in Amalthea."
  :type 'integer
  :group 'amalthea)

(defcustom amalthea-tab-width 2
  "The default width for indentation (in spaces) in Amalthea."
  :type 'integer
  :group 'amalthea)

(setq-default indent-tabs-mode nil
              tab-width amalthea-tab-width
	            fill-column amalthea-fill-width
	            compilation-scroll-output 'first-error)

;; Enable `auto-fill-mode' for any and all `text-mode' major modes.
(add-hook 'text-mode-hook #'auto-fill-mode)

;; Automatically create a closing parenthesis/etc
(add-hook 'prog-mode-hook #'electric-pair-mode)

(use-package help
  :commands temp-buffer-resize-mode
  :general
  (amalthea-leader
   :keymaps 'normal
   "h" '(:ignore t :which-key "help")
   "h f" '(describe-function :which-key "describe function")
   "h v" '(describe-variable :which-key "describe variable"))
  :custom
  (help-window-select t "Automatically go to help window")
  :config (temp-buffer-resize-mode))

(use-package paren
  :commands (show-paren-mode)
  :init (show-paren-mode t)
  :custom
  (show-paren-delay 0 "Show matching parenthesis without delay.")
  (show-paren-highlight-openparen t "Always show the matching parenthesis.")
  (show-paren-when-point-inside-paren t "Show parenthesis when inside a block."))

(provide 'core-editor)

;;; core-editor.el ends here
