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
(use-package evil
  :demand t
  :init
  (progn
    (setq evil-want-integration nil     ;; Don't load this, we'll be using evil-collection
          evil-search-module 'swiper)   ;; Use Swiper for searches
    (evil-mode))
  :general
  (general-imap "j"  (general-key-dispatch 'self-insert-command
                       :timeout 0.25
                       "k" 'evil-normal-state)))

;;; `evil-collection':
;; Instead of having to try to consistently create a key theme for a ton of
;; various packages on my own, the Emacs and Evil community came together to
;; create `evil-collection', which contains a ton of packages and modes with
;; keybindings configured to match what you'd expect from Vim/Evil.
(use-package evil-collection
  :after evil
  :commands evil-collection-init
  :init
  (progn
    (setq evil-collection-setup-minibuffer t)
    (evil-collection-init)))

;;; `evil-lion':
;; Ever wanted to align a long bunch of variables at their equal signs? Look no
;; further, because that is exactly what this does.
(use-package evil-lion
  :commands evil-lion-mode
  :config (evil-lion-mode))

;;; `evil-commentary':
;; Quickly comment out a single line or a region. It's really neat.
(use-package evil-commentary
  :delight
  :commands evil-commentary-mode
  :init (evil-commentary-mode))

;;; `evil-surround':
;; Incredibly handy package, if you want to change what surrounds a text you can
;; use this to easily do that. Change `[' and it's closing brother to a pair of
;; `()'? `cs[(' and you're done.
(use-package evil-surround
  :commands global-evil-surround-mode
  :init (global-evil-surround-mode))

;;; `evil-goggles':
;; Show visual hints for what the action you just did. It's hard to tell without
;; explaining it, I recommend you check out the README on GitHub.
(use-package evil-goggles
  :delight
  :commands evil-goggles-mode
  :init (evil-goggles-mode))

(provide 'base-evil)
;;; base-evil.el ends here
