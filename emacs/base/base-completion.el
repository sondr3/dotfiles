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
  :init (ivy-mode 1)
  :config
  (csetq ivy-use-virtual-buffers t
         enable-recursive-minibuffers t
         ivy-count-format "%d/%d "))

;;; `Counsel':
;; Counsel is built on top of Ivy and contains a bunch of improved interfaces
;; for mechanisms in Emacs, like finding files or opening recent files etc.
(use-package counsel
  :demand t
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
    "a u" '(counsel-unicode-char :wk "find unicode symbol")
    "b b" '(ivy-switch-buffer :wk "change buffer")
    "f f" '(find-file :wk "find file")
    "f r" '(recentf :wk "find recent")
    "f s" '(save-buffer :wk "save buffer")))

;;; `amx':
;; If you've ever heard of `smex', `amx' is an actually updated and maintained
;; fork of that. It's basically a much better `M-x' that also works with Ivy. It
;; also retains history of previously run commands, which is really useful.
(use-package amx
  :commands amx-mode
  :after ivy
  :init (amx-mode))

;;; `Swiper':
;; This is just a straight upgrade of the default search in Emacs. Use it and
;; love it.
(use-package swiper
  :general
  (general-define-key "C-s" 'swiper)
  (general-nmap "/" 'swiper)
  (amalthea-leader "/" 'swiper))

;;; `hydra':
;; Extremely useful package for when you want to be able to be able to call
;; commands in succession without quitting whatever it is you're doing.
(use-package hydra)

;;; `Company':
;; Instead of using something like `auto-complete' we'll use `Company' to give
;; us auto completion for variables, functions and so on.
(use-package company
  :delight "Ⓐ"
  :ghook 'prog-mode-hook
  :init
  (csetq company-idle-delay 0.2              ;; How long to wait before popping up
         company-tooltip-limit 20            ;; Limit on how many options to displa
         company-show-numbers t              ;; Show numbers behind options
         company-tooltip-align-annotations t ;; Align annotations to the right
         company-require-match nil           ;; Allow free typing
         company-selection-wrap-around t     ;; Wrap around to beginning when you hit bottom of suggestions
         company-dabbrev-ignore-case nil     ;; Don't ignore case when completing
         company-dabbrev-downcase nil        ;; Don't automatically downcase competions
         company-dabbrev-other-buffers t))    ;; Search other buffers for completion candidates

;;; `company-quickhelp':
;; When idling on a chosen completion candidate, show the items help in a popup
;; box next to the completion window.
(use-package company-quickhelp
  :after company
  :commands company-quickhelp-mode
  :init (csetq company-quickhelp-use-propertized-text t) ;; Allow text to have properties like size, color etc
  :config (company-quickhelp-mode))

;;; `company-statistics':
;; When completing a candidate, save the candidate to a history file and sort
;; completions accordingly next time so the candidate is ranked higher than the
;; last time. Useful for when there are many options but you mostly select one
;; or a few of them.
(use-package company-statistics
  :after company
  :ghook 'company-mode-hook)

;;; Snippets

;;; `yasnippet':
;; Enables snippets and expansion of snippets with this package, we've also
;; included `yasnippet-snippets' for a whole lotta snippets that you can use.
;; TODO: This package slows down start-up a lot.
(use-package yasnippet
  :commands (yas-global-mode)
  :delight (yas-minor-mode "Ⓨ")
  :init
  (progn
    (csetq yas-snippet-dirs (list (expand-file-name "snippets" amalthea-emacs-dir)))
    (yas-global-mode))
  :general
  (amalthea-major-leader
    "Y" '(:ignore t :wk "Yasnippet")
    "Y c" '(yas-new-snippet :wk "create")
    "Y e" '(yas-expand :wk "expand")
    "Y i" '(yas-insert-snippet :wk "insert")
    "Y p" '(ivy-yasnippet :wk "preview")
    "Y r" '(yas-reload-all :wk "reload")
    "Y v" '(yas-visit-snippet-file :wk "edit")))

;;; `yasnippet-snippets':
;; Minor tweak to allow it to automatically load snippets, but only after the
;; actual package has been loaded. Otherwise it doesn't load personal snippets.
(use-package yasnippet-snippets
  :after yasnippet)

;;; `ivy-yasnippet':
;; This gives you an Ivy-powered way to preview your snippets by interactively
;; seeing how they would look.
(use-package ivy-yasnippet
  :after yasnippet)

(provide 'base-completion)
;;; base-completion.el ends here
