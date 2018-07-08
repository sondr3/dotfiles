;;; base-editor.el --- Base editor settings -*- lexical-binding: t -*-

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

;; Contains configuration for base packages related to editing, like emulating
;; Vim with `Evil', and a few others.

;;; Code:

;;; `Evil':
;; Configures evil-mode.
(use-package evil
  :demand t
  :init (setq-am evil-want-integration nil "Don't load this, we'll be using evil-collection")
  :config (evil-mode))

;;; `evil-collection':
;; Instead of having to try to consistently create a key theme for a ton of
;; various packages on my own, the Emacs and Evil community came together to
;; create `evil-collection', which contains a ton of packages and modes with
;; keybindings configured to match what you'd expect from Vim/Evil.
(use-package evil-collection
  :after evil
  :config (evil-collection-init))

;;; General programming:

;;; `rainbow-delimiters':
;; This is fairly straight forward, it matches pairs of parens with colors,
;; making it easier to at a glance see blocks of code.
(use-package rainbow-delimiters
  :commands (rainbow-delimiters-mode)
  :ghook ('prog-mode-hook #'rainbow-delimiters-mode))

;;; `aggressive-indent':
;; The default indentation mode for Emacs is okay, but when editing LISP you can
;; do so much more. Since it's not whitespace sensitive you're free to
;; manipulate it at will with packages like `smartparens' or `lispy'. This minor
;; mode aggressively indents code whenever you change any part of a code block.
(use-package aggressive-indent
  :delight
  :commands (aggressive-indent-mode)
  :ghook ('emacs-lisp-mode-hook #'aggressive-indent-mode))

(provide 'base-editor)

;;; base-editor.el ends here
