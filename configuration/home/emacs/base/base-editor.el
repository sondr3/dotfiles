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

;; Configures base editor settings, mostly with packages that I consider to be
;; useful for everyday configuration for all editing needs.

;;; Code:

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

;;; General programming:

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

(provide 'base-editor)
;;; base-editor.el ends here
