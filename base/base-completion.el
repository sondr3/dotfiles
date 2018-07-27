;;; base-completion.el --- Auto-completion -*- lexical-binding: t -*-

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

;; Provides auto completion powered by `Company', snippet expansions powered by
;; `Yasnippet' and `hippie-expand'.

;; We'll also be replacing the built-in auto-completion framework that comes
;; with Emacs, even `ido' with `Ivy'. I find that the default way that Emacs
;; does completion of filenames and commands is pretty bad, especially compared
;; to what `Counsel' can do. You could also use `Helm', but I like the more
;; minimalist approach that `Ivy' has.

;;; Code:

;;; `Ivy', `Counsel' and `Swiper':

;;; `Ivy':
;; Ivy is the generic auto completion frontend that we'll be using for
;; completion instead of the built-in mechanisms in Emacs.
(use-package ivy
  :demand t
  :commands ivy-mode
  :delight
  :config
  (progn
    (ivy-mode)
    (setq ivy-use-virtual-buffers t
          enable-recursive-minibuffers t
          ivy-count-format "%d/%d ")))

;;; `Counsel':
;; Counsel is built on top of Ivy and contains a bunch of improved interfaces
;; for mechanisms in Emacs, like finding files or opening recent files etc.
(use-package counsel
  :demand t
  :commands (counsel-mode)
  :delight
  :general
  (:keymaps 'ivy-mode-map
            [remap find-file]                'counsel-find-file
            [remap recentf]                  'counsel-recentf
            [remap imenu]                    'counsel-imenu
            [remap bookmark-jump]            'counsel-bookmark
            [remap execute-extended-command] 'counsel-M-x
            [remap describe-function]        'counsel-describe-function
            [remap describe-variable]        'counsel-describe-variable
            [remap describe-face]            'counsel-describe-face
            [remap eshell-list-history]      'counsel-esh-history)
  (amalthea-leader
    :keymaps 'normal
    "A u" '(counsel-unicode-char :which-key "find unicode symbol")
    "b b" '(ivy-switch-buffer :which-key "change buffer")
    "f f" '(find-file :which-key "find file")
    "f r" '(recentf :which-key "find recent")
    "f s" '(save-buffer :which-key "save buffer")))

;;; `Swiper':
;; This is just a straight upgrade of the default search in Emacs. Use it and
;; love it.
(use-package swiper
  :general
  (general-define-key "C-s" 'swiper)
  (general-nmap "/" 'swiper))

;;; `Company':
;; Instead of using something like `auto-complete' we'll use `Company' to give
;; us auto completion for variables, functions and so on.
(use-package company
  :delight " Ⓒ"
  :hook (prog-mode . company-mode)
  :init
  (setq-am company-idle-delay 0.2 "How long to wait before popping up")
  (setq-am company-tooltip-limit 20 "Limit on how many options to display")
  (setq-am company-show-numbers t "Show numbers behind options")
  (setq-am company-tooltip-align-annotations t "Align annotations to the right")
  (setq-am company-require-match nil "Allow free typing")
  (setq-am company-selection-wrap-around t "Wrap around to beginning when you hit bottom of suggestions")
  (setq-am company-dabbrev-ignore-case nil "Don't ignore case when completing")
  (setq-am company-dabbrev-downcase nil "Don't automatically downcase competions")
  (setq-am company-dabbrev-other-buffers t "Search other buffers for completion candidates")
  :config (company-tng-configure-default))

;;; `company-quickhelp':
;; When idling on a chosen completion candidate, show the items help in a popup
;; box next to the completion window.
(use-package company-quickhelp
  :after company
  :commands company-quickhelp-mode
  :config
  (progn
    (setq-am company-quickhelp-use-propertized-text t "Allow text to have properties like size, color etc")
    (company-quickhelp-mode)))

;;; `company-statistics':
;; When completing a candidate, save the candidate to a history file and sort
;; completions accordingly next time so the candidate is ranked higher than the
;; last time. Useful for when there are many options but you mostly select one
;; or a few of them.
(use-package company-statistics
  :after company
  :ghook 'company-mode-hook
  :config
  (setq-am company-statistics-file (concat amalthea-cache-dir "company-statistics.el") "Location to save statistics"))

;;; Snippets

;;; `yasnippet':
;; Enables snippets and expansion of snippets with this package, we've also
;; included `yasnippet-snippets' for a whole lotta snippets that you can use.
;; TODO: This package slows down loading a lot.
(use-package yasnippet
  :delight (yas-minor-mode " ⓨ")
  :ghook ('(text-mode-hook prog-mode-hook snippet-mode-hook) #'yas-minor-mode))

(provide 'base-completion)
;;; base-completion.el ends here
