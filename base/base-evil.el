;;; base-evil.el --- Evil configuration -*- lexical-binding: t -*-

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

;; Configures `Evil' and all it's ilk.

;;; Code:

;;; `Evil':
;; Configures evil-mode.
(general-imap "j" (general-key-dispatch 'self-insert-command
                    :timeout 0.25
                    "k" 'evil-normal-state))
(setq evil-want-keybinding nil            ;; Same as above
      evil-search-module 'swiper          ;; Use Swiper for searches
      evil-collection-setup-minibuffer t) ;; Evil bindings in the minibuffer
(require 'evil)
(evil-mode)

;;; `evil-collection':
;; Instead of having to try to consistently create a key theme for a ton of
;; various packages on my own, the Emacs and Evil community came together to
;; create `evil-collection', which contains a ton of packages and modes with
;; keybindings configured to match what you'd expect from Vim/Evil.
(require 'evil-collection)
(with-eval-after-load 'evil
  (evil-collection-init))

;;; `evil-lion':
;; Ever wanted to align a long bunch of variables at their equal signs? Look no
;; further, because that is exactly what this does.
(require 'evil-lion)
(with-eval-after-load 'evil
  (evil-lion-mode))

;;; `evil-commentary':
;; Quickly comment out a single line or a region. It's really neat.
(require 'evil-commentary)
(with-eval-after-load 'evil
  (delight 'evil-commentary-mode nil "evil-commentary")
  (evil-commentary-mode))

;;; `evil-surround':
;; Incredibly handy package, if you want to change what surrounds a text you can
;; use this to easily do that. Change `[' and it's closing brother to a pair of
;; `()'? `cs[(' and you're done.
(require 'evil-surround)
(with-eval-after-load 'evil
  (global-evil-surround-mode))

;;; `evil-goggles':
;; Show visual hints for what the action you just did. It's hard to tell without
;; explaining it, I recommend you check out the README on GitHub.
(require 'evil-goggles)
(with-eval-after-load 'evil
  (delight 'evil-goggles-mode nil "evil-goggles")
  (evil-goggles-mode))

;;; `evil-smartparens`:
;; Whenever we use `smartparens` we also want to ensure that we enable the
;; corresponding evil counterpart so things works as we expect.
(require 'evil-smartparens)
(with-eval-after-load 'evil
  (delight 'evil-smartparens-mode nil "evil-smartparens")
  (general-add-hook 'smartparens-enabled-hook #'evil-smartparens-mode))

(provide 'base-evil)
;;; base-evil.el ends here
