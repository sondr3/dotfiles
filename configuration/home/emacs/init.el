;;; init.el --- Amalthea configuration -*- lexical-binding: t -*-

;; Author: Sondre Nilsen
;; Maintainer: Sondre Nilsen
;; Homepage: https://github.com/sondr3/amalthea

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

;; Isn't the view from here beautiful?

;;; Code:

;;;; Initialization
;; Compatibility with versions 26 and below.
(unless (boundp 'early-init-file)
  (load (expand-file-name "early-init" user-emacs-directory)))

;; Check for Emacs version
(eval-when-compile
  (unless (not (version< emacs-version "26"))
    (error "You are using Emacs %s, Amalthea requires version 26 or higher" emacs-version)))

;;;; Core configuration

(defvar amalthea-emacs-dir (eval-when-compile (file-truename user-emacs-directory))
  "Path to the current Emacs directory.")

(defvar amalthea-dotfiles-dir (expand-file-name ".dotfiles" (getenv "HOME"))
  "Location of dotfiles for Amalthea.")

;;;;; Amalthea group and customizations

(defgroup amalthea nil
  "Amalthea settings and configurations."
  :group 'convenience
  :prefix "amalthea")

(defcustom amalthea-mono-font "PragmataPro Mono Liga"
  "The default monospaced font that Amalthea uses."
  :type 'string
  :group 'amalthea)

(defcustom amalthea-serif-font "PragmataPro Liga"
  "The default sans serif font that Amalthea uses."
  :type 'string
  :group 'amalthea)

(defcustom amalthea-line-spacing 0.15
  "The default line spacing width that Amalthea uses."
  :type 'number
  :group 'amalthea)

(defcustom amalthea-font-size 140
  "The default font size for Amalthea."
  :type 'number
  :group 'amalthea)

;;;;; Core utilities

(defun amalthea--byte-compile-amalthea ()
  "Byte compile all files and directories used in Amalthea."
  (interactive)
  (dolist (file (list (expand-file-name "early-init.el" amalthea-emacs-dir)
                      (expand-file-name "init.el" amalthea-emacs-dir)))
    (byte-compile-file file)))

(defmacro csetq (&rest body)
  "A simple and better version of `setq' that also respects if a
  variable has a `custom-set' property. Works just like the good
  old version, but better, because you can also add comments to
  assignments."
  `(progn
     ,@(cl-loop for (var val) on body by 'cddr
                collect `(funcall (or (get ',var 'custom-set) #'set)
                                  ',var ,val))))

;;;;; Settings

;; Emacs actually predates UTF8, which to my mind is kinda nuts. So we'll force
;; Emacs to always use unicode characters and UTF8 everywhere.
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system                   'utf-8)
(set-terminal-coding-system             'utf-8)
(set-keyboard-coding-system             'utf-8)
(set-selection-coding-system            'utf-8)
(setq locale-coding-system              'utf-8)
(setq-default buffer-file-coding-system 'utf-8)

;; Then we can set some defaults that are core to Emacs. Beginning with changing
;; the initial screen for Emacs.
(setq-default inhibit-startup-message t
              inhibit-startup-buffer-menu t
              inhibit-startup-screen t
              inhibit-startup-echo-area-message t
              initial-buffer-choice t
              initial-major-mode 'fundamental-mode
              ;; Recusion and lisp call limit
              max-lisp-eval-depth 50000
              max-specpdl-size 10000
              ;; Set some common sense settings
              custom-file (expand-file-name "custom.el" amalthea-emacs-dir)
              byte-compile--use-old-handlers nil     ;; Use the most recent byte code ops
              sentence-end-double-space nil          ;; Sentences end with a single space
              vc-follow-symlinks t                   ;; Always follow symbolic links
              save-interprogram-paste-before-kill t) ;; Save paste history when killing Emacs)

;; Hide the backup files inside `no-littering' directories.
(csetq backup-by-copying t            ;; Don't clobber symlinks
       delete-old-versions t          ;; Silently delete old files
       delete-by-moving-to-trash t    ;; And move them into the trash can
       kept-new-versions 6            ;; Keep some new versions around
       kept-old-versions 2            ;; But just a few old ones
       version-control t              ;; use versioned backups
       auto-save-file-name-transforms
       `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)) ;; Store auto save files in `no-littering'
       backup-directory-alist
       `((".*" . ,(no-littering-expand-var-file-name "backup/"))))   ;; Store backups in `no-littering'

;; Fully inhibit the initial screen
(fset #'display-startup-echo-area-message #'ignore)

;; Mostly to save at most two strokes and at a minimum one. Efficiency baby.
(fset #'yes-or-no-p #'y-or-n-p)

;; Only load `custom.el' if it exists
(when (file-exists-p custom-file)
  (load custom-file t t))

;;;;; Packaging
;; There's a ton of different ways of doing package management in Emacs, most of
;; which nowadays revolve around `use-package'. There's also `quelpa',
;; `straight.el', `Borg' and a bunch more. I've tried them all, but there is
;; always some minute thing about them that bothers me. In the end I've ended up
;; configuring the packages I need in a `Nix' expression, allowing me to
;; declarative install dependencies.

;; I still use `use-package' to configure my installed packages, but the
;; `:ensure' functionality of it is disabled. This gives me the best of both
;; worlds, all the great functionality and integration of `use-package' and the
;; declarative packaging that you would have with `straight.el' or `Borg'.

;; Require `use-package', need I say more?
(eval-and-compile
  (require 'use-package))

(csetq use-package-ensure-function 'ignore ;; We don't want to install packages with `use-package'
       use-package-expand-minimally t      ;; Expand the `use-package' with no bells or whistles
       use-package-always-defer t)         ;; Always defer packages

;;; `delight':
;; Though you could use `diminish' for making the modeline look better,
;; `delight' is a much better package. Not only can you change the names or hide
;; major-modes from the modeline, you can also nest what minor-mode you want to
;; hide instead of having to do it one at a time.
(use-package delight :demand t)

;;;;; System configuration(s)
;; Contains settings related to making Emacs work better on various operating
;; systems.

;; Jupiter (macOS): Enables emojis to be properly rendered, makes it so the
;; titlebar is dark and not transparent and fixes a few related frame issues.
;; Also fixes and enables smoother scrolling for macOS.

;; Neptune (NixOS): Configures and enables copying and pasting between Emacs and
;; X11 and choses the builtin tooltips over GTK.

(csetq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING) ;; Magic voodoo
       select-enable-clipboard t                                      ;; Cut and paste from the actual clipboard
       select-enable-primary t)                                       ;; Use the primary clipboard

(cond
 ((string= (system-name) "jupiter") ;; macOS configuration
  (progn
    (set-fontset-font "fontset-default" 'unicode "Apple Color Emoji" nil 'prepend)
    (dolist (pair '((ns-transparent-titlebar . nil)
                    (ns-appearance . dark)))
      (push pair (alist-get 'ns window-system-default-frame-alist nil))
      (set-frame-parameter nil (car pair) (cdr pair)))

    (use-package exec-path-from-shell
      :commands exec-path-from-shell-initialize
      :init (exec-path-from-shell-initialize)) ;; Make sure $PATH is correct on macOS

    (csetq ns-use-thin-smoothing nil    ;; Don't use thinner strokes on macOS
           mouse-wheel-flip-direction t ;; Change scrolling to new macOS defaults
           mouse-wheel-tilt-scroll t))) ;; Change scrolling to new macOS defaults

 ((string= (system-name) "neptune") ;; Linux configuration
  (progn
    (defvar x-gtk-use-system-tooltips nil)
    (csetq x-gtk-use-system-tooltips nil     ;; Use the builtin Emacs tooltips
           x-underline-at-descent-line t     ;; Fix for not using GTK tooltips
           amalthea-font-size 120            ;; Make the font smaller on Neptune
           amalthea-line-spacing 0.10))))    ;; And the spacing a little less

;;;;; Interface
;; Configurations for making Emacs and Amalthea a better looking editor,
;; something that Emacs sorely needs, because it's pretty damn ugly by default.

;;;;;; Fonts
(set-face-attribute 'default nil
		                :family amalthea-mono-font
		                :height amalthea-font-size)
(set-face-attribute 'variable-pitch nil
		                :family amalthea-serif-font
		                :height amalthea-font-size)
(set-frame-font amalthea-mono-font nil t)

;;;;;; General UI settings:
;; Starting with Emacs 26.1, Emacs has a built-in line number mode written in C
;; that supports relative line numbers, like in Vim. This is awesome, because
;; the previous offerings had a whole bunch of drawbacks, either being really
;; slow, or not properly supporting relative line numbers in Org mode and so on.
;; Thankfully we now have a proper solution.

;; First, we set the relative line number to `visual', which counts the visible
;; lines on the screen. Otherwise headers that are folded in Org makes the line
;; count go haywire, next we set a default width for line numbers and ensure
;; that it doesn't get narrower or wider depending on the amount of lines in a
;; file.

;; Finally, we disable the visible and audible bell that is enabled by default
;; because it's bloody annoying.
(setq-default line-spacing amalthea-line-spacing      ;; Give lines some breathing room
              frame-title-format '("Amalthea ∷ %b")  ;; Name the Emacs window
              display-line-numbers 'visual            ;; Count the visible line numbers, not the actual number
              display-line-numbers-current-absolute t ;; Show line number of current line instead of 0
              display-line-numbers-width 4            ;; Enough space for huge files
              display-line-numbers-widen nil          ;; Disable dynamic sizing of line number width
              visible-bell nil                        ;; No bells
              ring-bell-function #'ignore)            ;; NO BELLS

;;; `hl-line':
;; This is basically something that I learned to use and love from Vim, it
;; highlights the current line where the cursor is currently active. We enable
;; this minor mode globally, and then make it so it doesn't display in inactive
;; windows.
(use-package hl-line
  :commands global-hl-line-mode
  :init (global-hl-line-mode)
  :config (csetq global-hl-line-sticky-flag nil)) ;; Don't highlight current line in inactive buffers

;;;;; Keybindings
;; This is probably the hardest thing by far to configure and properly do in
;; Emacs, at least in my opinion. I could use something like Spacemacs or Doom
;; which has a proper consistent theme for keybindings, but that's no fun.
;; Instead we'll roll our own built around `Evil', `General.el' and `which-key'.
;; Lastly, we'll mimick how I used to do things in Vim (and how Spacemacs and
;; others does things) by letting `SPC' be our leader key and `,' be our major
;; mode leader key. If you are in the `insert' state, you can use `C-SPC' for
;; the leader key and `M-,' for the major mode leader key.

;; Custom bindings
(defvar amalthea-leader-key "SPC"
  "The default leader key for Amalthea.")

(defvar amalthea-leader-secondary-key "C-SPC"
  "The secondary leader key for Amalthea.")

;;; `which-key':
;; This is a really cool package, I initially discovered this from Spacemacs (as
;; I have done with a great many things). What it does is show you any and all
;; keybindings you can complete from the binding you just executed. For example,
;; if you are in Org-mode and run `C-c', `which-key' will show on the bottom of
;; the screen and show all the keybindings you can complete from there. It's
;; really great for discoverability.
(use-package which-key
  :demand t
  :delight
  :commands which-key-mode
  :init (which-key-mode)
  :config
  (progn
    (csetq which-key-idle-delay 0.2                           ;; Reduce the time before which-key pops up
           which-key-allow-evil-operators t                   ;; Show evil keybindings
           which-key-sort-order 'which-key-key-order-alpha))) ;; Sort things properly alphabetical

;;; `General':
;; This is a whole framework for binding keys in a really nice and consistent
;; manner. It also enables us to configure our leader keys using the constants
;; we created in the introduction to keybindings.
(use-package general
  :demand t
  :commands general-evil-setup
  :config
  (progn
    (general-evil-setup)
    (general-create-definer amalthea-leader
      :states '(normal insert emacs)
      :prefix amalthea-leader-key
      :non-normal-prefix amalthea-leader-secondary-key)))

;; Default `which-key' prefixes
;; This keeps all the main menus in one place instead of spread throughout the
;; whole project.
(amalthea-leader
  "SPC" '(counsel-M-x :wk "M-x")
  "a" '(:ignore t :wk "applications")
  "b" '(:ignore t :wk "buffers")
  "f" '(:ignore t :wk "files")
  "g" '(:ignore t :wk "git")
  "h" '(:ignore t :wk "help")
  "S" '(:ignore t :wk "spelling")
  "w" '(:ignore t :wk "windows"))

;;;;; Editor
;; Contains configuration and settings for packages that are what I'd consider
;; core to a proper usage of Emacs.

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

;;;;; Evil
;; Configures `Evil' and all it's ilk.

;;; `Evil':
;; Configures evil-mode.
(use-package evil
  :demand t
  :general
  (general-imap "j"  (general-key-dispatch 'self-insert-command
                       :timeout 0.25
                       "k" 'evil-normal-state))
  :init
  (progn
    (csetq evil-want-integration t      ;; `evil-collection' compatability fix
           evil-want-keybinding nil     ;; Same as above
           evil-search-module 'swiper)) ;; Use Swiper for searches
  :config (evil-mode))

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

;;;;; Editor (base)
;; Configures base editor settings, mostly with packages that I consider to be
;; useful for everyday configuration for all editing needs.

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

;;;;; Completion
;; Provides auto completion powered by `Company', snippet expansions powered by
;; `Yasnippet' and `hippie-expand'.

;; We'll also be replacing the built-in auto-completion framework that comes
;; with Emacs, even `ido' with `Ivy'. I find that the default way that Emacs
;; does completion of filenames and commands is pretty bad, especially compared
;; to what `Counsel' can do. You could also use `Helm', but I like the more
;; minimalist approach that `Ivy' has.

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
         company-dabbrev-downcase nil        ;; Don't automatically downcase completions
         company-dabbrev-other-buffers t))   ;; Search other buffers for completion candidates

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
    (yas-global-mode)))

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

;;;;; Git
;; Like pretty much everybody nowadays I'm using ~git~, and with that comes
;; probably one of the absolutely best packages that exists for Emacs: Magit!
;; Even if you're okay at using git from the command line, Magit just blows the
;; command line interface for git out of the water. If you haven't tried it I
;; highly recommend it.

;;; `git-hydra':
;; Quickly move between hunks in your document.
(defhydra hydra-git (:color pink)
  "git"
  ("k" diff-hl-previous-hunk "prev hunk")
  ("j" diff-hl-next-hunk "next hunk")
  ("q" nil "quit" :color blue))

;;; `Magit':
;; Enable and appreciate it! The only thing we'll really change is adding a few
;; extra functions and hooks to work better with Borg.
(use-package magit
  :delight auto-revert-mode
  :commands magit-add-section-hook
  :general
  (amalthea-leader
    "g h" '(hydra-git/body :wk "hydra")
    "g s" '(magit-status :wk "git status")))

;;; `git-modes':
;; A few minor major modes for editing `.gitignore', `.gitattributes' and
;; `.gitconfig' files.
(use-package gitignore-mode)
(use-package gitattributes-mode)
(use-package gitconfig-mode)

;;; `evil-magit':
;; Magit by default doesn't include any Evil keybindings, which makes sense but
;; is kinda required since we use Evil.
(use-package evil-magit
  :after magit
  :commands evil-magit-init
  :init (evil-magit-init))

;;; `diff-hl':
;; There's a plugin for Vim called GitGutter that is really neat, in the fringe
;; of your file it shows where hunks have been changed, added and removed from
;; the file. There's a similarly named plugin for Emacs, but it hasn't been
;; updated for quite a while and even then, `diff-hl' is quite a lot better than
;; it is. There's no magic here, we'll enable it globally, hook into Magit so
;; that diff-hl updates when we commit using Magit.
(use-package diff-hl
  :commands (diff-hl-magit-post-refresh global-diff-hl-mode)
  :functions (diff-hl-flydiff-mode diff-hl-margin-mode)
  :defines diff-hl-margin-symbols-alist
  :general
  (amalthea-leader
    "g j" '(diff-hl-next-hunk :wk "next hunk")
    "g k" '(diff-hl-previous-hunk :wk "previous hunk"))
  :init
  (progn
    (csetq diff-hl-margin-symbols-alist
           '((insert . "+") (delete . "-") (change . "~")
             (unknown . "?") (ignored . "i")))
    (global-diff-hl-mode)
    (diff-hl-margin-mode)
    (diff-hl-flydiff-mode)
    (general-add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)))

;;; `hl-todo':
;; This is a really simple mode that highlights things that are marked as TODO,
;; FIXME and so on. It's quite useful if you like to litter your project with
;; them.
(use-package hl-todo
  :commands global-hl-todo-mode
  :init (global-hl-todo-mode))

;;;;; Projectile
;; Configures Projectile for some sweet, sweet project awesomeness.

;;; `projectile':
;; Projectile is a program for working with projects in Emacs, it supports a ton
;; of features out of the box that are awesome and useful, like searching for
;; files only in the current project, recent files in current project and so on.
(use-package projectile
  :commands projectile-mode
  :delight " Ⓟ"
  :general
  (amalthea-leader
    "p" '(projectile-command-map :wk "project"))
  :init
  (progn
    (csetq projectile-completion-system 'ivy ;; Use Ivy for completion
           projectile-sort-order 'recentf    ;; Sort by using `recentf'
           projectile-enable-caching t))     ;; Enable caching to speed up searching, finding files
  :config (projectile-mode))

;;; `counsel-projectile':
;; Even though we've configured Projectile to use Ivy, we can extend it even
;; more by also using Counsel too.
(use-package counsel-projectile
  :after projectile
  :commands counsel-projectile-mode
  :init (counsel-projectile-mode))

;;;;; Checking
;; Syntax and spell checking for both programming and regular languages, using
;; `flyspell' and `flycheck' to configure them.

;;; `spelling-hydra':
;; A hydra for quickly moving through your buffer, moving from one error to the
;; other, checking and correcting them as you go. Also enables toggling of
;; either straight up `flyspell' or of it's `prog-mode'.
;;
;; Ever so lightly stolen from rmberYou
(defhydra hydra-spelling (:color blue)
  "
  ^
  ^Errors^            ^Checker^            ^Mode^
  ^──────^─────────── ^───────^─────────── ^────^─────────
  _k_: previous       _f_: check           _m_: mode
  _j_: next           _c_: correction      _p_: prog mode
  ^^                  _d_: dictionary      ^^
  ^^                  ^^                   ^^
  "
  ("q" nil "quit")
  ("k" flyspell-correct-previous :color pink)
  ("j" flyspell-correct-next :color pink)
  ("c" ispell)
  ("d" ispell-change-dictionary)
  ("f" flyspell-buffer)
  ("m" flyspell-mode)
  ("p" flyspell-prog-mode))

;;; `flyspell':
;; The builtin spell checker for Emacs, this is a really nice little package
;; that automatically does it's magic whenever it's needed. For programming
;; modes we use the builtin `prog-mode' version of Flyspell, and we then just
;; enable the regular version for `text-mode' buffers.
(use-package flyspell
  :commands (flyspell-mode flyspell-prog-mode)
  :delight "Ⓢ"
  :ghook ('prog-mode-hook #'flyspell-prog-mode)
  :ghook ('text-mode-hook #'flyspell-mode)
  :general
  (amalthea-leader
    "S s" '(hydra-spelling/body :wk "hydra")
    "S b" '(flyspell-buffer :wk "spell check buffer")
    "S n" '(flyspell-goto-next-error :wk "next spelling error"))
  :init
  (progn
    (csetq ispell-program-name "aspell"
           ispell-local-dictionary "en_US"
           flyspell-use-meta-tab nil
           flyspell-issue-message-flag nil
           flyspell-issue-welcome-flag nil)))

;;; `flyspell-correct':
;; The default correction window for Flyspell is awful, terribly so actually, so
;; we'll use a package to fix this. This creates a generic way of correcting
;; words and we'll use a Ivy-minibuffer to correct wording.
;; TODO Find a better way to get to the Ivy menu in Flyspell
(use-package flyspell-correct-ivy
  :after flyspell
  :commands (flyspell-correct-word-generic
             flyspell-correct-ivy
             flyspell-correct-previous)
  :general
  (amalthea-leader
    "S c" '(flyspell-correct-previous :wk "correct prev word")
    "S C" '(flyspell-correct-next :wk "correct next word")
    :init (csetq flyspell-correct-interface #'flyspell-correct-ivy)))

;;; `flycheck':
(use-package flycheck
  :delight "Ⓒ"
  :commands global-flycheck-mode
  :init (global-flycheck-mode))

;;;;; Window
;; Windows 10

(defhydra hydra-zoom (:color red :hint nil)
  "zoom"
  ("k" text-scale-increase "in")
  ("j" text-scale-decrease "out")
  ("r" (text-scale-adjust 0) "reset" :color blue)
  ("q" nil "quit" :color blue))

(amalthea-leader
  "w f" '(toggle-frame-fullscreen :wk "fill screen")
  "w m" '(toggle-frame-maximized :wk "maximize")
  "w z" '(hydra-zoom/body :wk "zoom"))

;;;;; UI
;;; Theme
;; You should probably change this, I have a very weird taste in themes.
(use-package apropospriate-theme
  :init (load-theme 'apropospriate-light t))

;; Fix the spacing between the major mode name and all the minor modes.
(catch 'done
  (mapc (lambda (x)
          (when (and (consp x)
                     (equal (cadr x) '("" minor-mode-alist)))
            (setcar (cadr x) " ∷ ")
            (throw 'done t)))
        mode-line-modes))

;;;;; Help
;; Configuration for making the help buffers more helpful.

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

;;;;; Search
;; Gives you super powered searching via `ripgrep' and `deadgrep'.

;;; `deadgrep':
;; Sweet, sweet searching.
(use-package deadgrep
  :general
  (amalthea-leader
    "a s" '(deadgrep :wk "ripgrep")))

;;;;; Shell
(defun vterm-init ()
  "Initialize vterm properly."
  (interactive "P")
  (toggle-truncate-lines t)
  (visual-line-mode 0))

(use-package vterm
  :commands (vterm)
  :ghook ('vterm-mode-hook (lambda ()
                             (toggle-truncate-lines t))))

;;;;; Modules
;;;;;; Programming languages
;;;;;;; LSP
;; Support for the Language Server Protocol and the various associated packages
;; that use/feed it.

;;; `lsp-mode':
;; The bread and butter for LSP, the only thing we'll configure is disabling
;; Flymake because we're using Flycheck instead.
(use-package lsp-mode
  :commands lsp
  :delight " Ⓛ"
  :init (csetq lsp-prefer-flymake nil))

;;; `lsp-ui':
;; Gives us some goodies while browsing the code.
(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :ghook ('lsp-mode-hook #'lsp-ui-mode))

;;; `company-lsp':
;; Enables auto-completion for languages that use LSP.
(use-package company-lsp
  :after company
  :commands company-lsp
  :init (push 'company-lsp company-backends))

;;;;;;; Assembly
;; Adds support for `nasm-mode' to supercede the builtin `asm-mode'.

(add-to-list 'auto-mode-alist '("\\.asm\\'" . nasm-mode))

;;;;;;; Elisp
;; Configuration for Emacs LISP, mostly adding a few hooks here and there and
;; making it so that any files are automatically byte compiled when there exists
;; a previously byte compiled version, and adding a macro expansion library.

;;; `emacs-lisp':
;; Adds auto compilation for packages and libraries on load and save if there
;; exists a newer version of it, adds `outline-minor-mode' and `reveal-mode' to
;; this mode and allows us to use `C-c e' to expand any code that contains
;; macros.
(use-package emacs-lisp
  :gfhook #'auto-compile-on-load-mode #'auto-compile-on-save-mode
  :ghook
  ('emacs-lisp-mode-hook #'reveal-mode)
  :init (csetq flycheck-emacs-lisp-load-path 'inherit))

;;; `outshine':
;; Gives you programs the goodies of navigating and folding headers like in
;; Org-mode.
(use-package outshine
  :delight " Ⓞ"
  :ghook ('emacs-lisp-mode-hook #'outshine-mode))

;;; `auto-compile':
;; Automatically compiles any `.el' files into their byte compiled version,
;; making sure everything is up to date.
(use-package auto-compile
  :commands (auto-compile-on-save-mode auto-compile-on-load-mode)
  :ghook ('auto-compile-inhibit-compile-hook #'auto-compile-inhibit-compile-detached-git-head)
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode)
  (csetq auto-compile-display-buffer nil             ;; Don't automatically show the *Compile Log* buffer
         auto-compile-mode-line-counter t            ;; Display number of warnings in modeline
         auto-compile-source-recreate-deletes-dest t ;; Delete leftover byte code when recompiling
         auto-compile-toggle-deletes-nonlib-dest t   ;; Delete non-library byte code
         auto-compile-update-autoloads t))           ;; Update autoloads after compiling

;;; `macrostep':
;; This is a hydra that we'll use together with the package itself, this makes
;; it really easy to quickly work your way through macros as you are working on
;; them or using them.
(defhydra hydra-macrostep (:color pink)
  "macrostep"
  ("q" macrostep-collapse-all "collapse all macros" :color blue)
  ("c" macrostep-collapse "collapse macro")
  ("e" macrostep-expand "expand macro")
  ("j" macrostep-next-macro "next macro")
  ("k" macrostep-prev-macro "prev macro"))

(use-package macrostep
  :functions (macrostep-collapse-all macrostep-collapse macrostep-next-macro macrostep-prev-macro)
  :general
  (emacs-lisp-mode-map
   "C-c m" 'hydra-macrostep/body))

;;; Hide some minor modes and rename the major mode
(delight '((emacs-lisp-mode "Elisp" :major)
           (eldoc-mode nil "eldoc")
           (outline-minor-mode nil "outline")
           (reveal-mode nil "reveal")))

;;;;;;; Haskell
;; Configuration for the Haskell language, this package requires you to have
;; `stack' installed, as `intero' uses it.

;;; `haskell-mode':
;; The main focal point of the Haskell editing experience, there's no magic
;; here. All it does is add some modes to `haskell-mode', exclude some project
;; files from `recentf' and set a few common sense settings.
(use-package haskell-mode
  :ghook ('haskell-mode-hook (list #'subword-mode #'haskell-auto-insert-module-template))
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
  :delight " λ"
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
(use-package hlint-refactor)

(delight '((haskell-mode "" :major)))

;;;;;;; Shell
;; Support for writing shell scripts in Bash, Fish and more. Nothing out of the
;; ordinary here folks.

;;; `fish-mode':
;; Because Fish is the superior shell.
(use-package fish-mode
  :init (csetq fish-indent-offset amalthea-tab-width))

;;; `sh-script':
;; Make sure we have the `shellcheck' package for linting of shell files and add
;; a bunch of key bindings to `sh-mode' for easier access, these are essentially
;; the same keys that are under `C-c'.
(use-package sh-script
  :ghook ('sh-mode-hook (list #'subword-mode #'flycheck-mode)))

;;; `company-shell':
;; Adds auto completion for shell scripting to Company.
(use-package company-shell
  :after company
  :init (add-to-list 'company-backends '(company-shell company-shell-env company-fish-shell)))

;;;;;;; Json
;; Simple configuration for editing and viewing JSON files.

;;; `json-mode':
;; This is really nothing fancy, we bind some keys and set the indentation to
;; it's proper size.
(use-package json-mode
  :init (csetq js-indent-level 2))
;;;;;;; Java
;; Because fuck me, right? No, I only use it so that indentation and
;; auto-completion works when taking notes, I would never, ever write Java in
;; Emacs. I don't hate myself /that/ much.

(general-add-hook 'java-mode-hook #'electric-pair-local-mode)

;;;;;;; Nix
;; Nix language support for Emacs, for more information, see
;; `https://nixos.org/'. This adds support for indenting and auto completion for
;; both `.nix' configuration files and NixOS services, programs etc.

;;; `nix-mode':
;; Adds support for editing and working with Nix expressions, we don't even need
;; configuration for this! Set it and forget it.
(use-package nix-mode)

;;;;;;; TypeScript
;; TypeScript support.

;;; `prettier-js':
;; Automatically prettify buffers.
(use-package prettier-js :delight " Ⓟ")

;;; `typescript-mode':
;; Quick configuration for TypeScript, setting the indentation and enabling
;; `prettier-js' and `lsp' for it.
(use-package typescript-mode
  :ghook ('typescript-mode-hook (list #'lsp #'prettier-js-mode))
  :init (csetq typescript-indent-level amalthea-tab-width
               js-indent-level amalthea-tab-width))

;;;;;; Tools
;;;;;;; Docker
;;; `dockerfile-mode':
;; Gives you syntax highlighting and completion for Docker.
(use-package dockerfile-mode
  :commands dockerfile-mode
  :mode "\\Dockerfile\\'")

;;;;;;; Nginx
(use-package nginx-mode)

(use-package company-nginx
  :after nginx-mode
  :ghook ('nginx-mode-hook #'company-nginx-keywords))
(with-eval-after-load 'nginx-mode
  (general-add-hook 'nginx-mode-hook #'company-nginx-keywords))

;;;;;; Text
;;;;;;; LaTeX
;; LaTeX gives amazing results but working with it can be really awful, I've
;; honed my configuration for working with it for a few years now and by now it
;; works very well. Though it will probably not work straight out of the box for
;; anyone but me. If you truly want to try it out, you must have the full TeX
;; Live distribution installed, you need to use `latexmk' with `lualatex' and
;; you should copy that configuration from my dotfiles. Once you've done that,
;; lets dive in.

;;; `auctex':
;; Emacs comes bundled with a very simply TeX-mode, however we want to enhance
;; this by using `auctex' to add a bunch of really nice quality of life changes.
(use-package tex
  :delight iimage-mode
  :commands (TeX-source-correlate-mode TeX-PDF-mode)
  :functions LaTeX-math-mode
  ;; Enable some extra modes for editing, spelling, auto completion etc
  :ghook ('LaTeX-mode-hook (list #'TeX-fold-mode #'LaTeX-math-mode #'TeX-source-correlate-mode
                                 #'TeX-PDF-mode #'flyspell-mode #'company-mode #'rainbow-delimiters-mode))
  :init
  (progn
    (setq-default TeX-master nil)                                 ;; Always ask which file is the master TeX file
    (csetq TeX-command-default "latexmk"                           ;; Use `latexmk' to compile documents
           TeX-command-force "latexmk"                             ;; REALLY use `latexmk' to compile documents
           TeX-engine 'lualatex                                    ;; The default engine of choice is `lualatex'
           TeX-auto-save t                                         ;; Save documents automatically when running commands on them
           TeX-parse-self t                                        ;; Don't really know, everyone sets it to `t'
           TeX-save-query nil                                      ;; Don't ask for permission when saving
           TeX-PDF-mode t                                          ;; Compile documents to PDF
           TeX-show-compilation nil                                ;; Don't pop up the compilation buffer, use C-c C-l to show it
           TeX-syntactic-comment t                                 ;; No idea either, no documentation for it
           TeX-clean-confirm t                                     ;; Ask before cleaning temporary files
           TeX-electric-math t                                     ;; Electric opening and closing of math environments
           TeX-electric-sub-and-superscript t                      ;; Same with sub and superscript
           TeX-source-correlate-mode t                             ;; Enable correlation between source and output
           TeX-source-correlate-method 'synctex                    ;; Use `synctex' to sync cursor location to PDF viewer
           TeX-source-correlate-start-server t                     ;; Start the server by default
           LaTeX-babel-hyphen nil                                  ;; Don't aid in hyphenation
           TeX-view-program-selection '((output-pdf "Zathura"))))) ;; View compiled PDFs in this program

;;; `auctex-latexmk':
;; Adds support for `latexmk' to `auctex', mostly useful for making sure that
;; various minor modes are added to the command line parameters used by
;; `latexmk'.
(use-package auctex-latexmk
  :commands auctex-latexmk-setup
  :init
  (progn
    (csetq auctex-latexmk-inherit-TeX-PDF-mode t) ;; Tell `auctex' that we're compiling to a PDF
    (auctex-latexmk-setup)))

;;; `company-auctex':
;; Auto completion for LaTeX buffers. Yes. It's good.
(use-package company-auctex
  :commands company-auctex-init
  :init (company-auctex-init))

;;; `company-math':
;; Adds auto completion for symbols and commands used in LaTeX.
(use-package company-math
  :config
  (progn
    (add-to-list 'company-backends 'company-math-symbols-latex t)
    (add-to-list 'company-backends 'company-math-symbols-unicode t)
    (add-to-list 'company-backends 'company-latex-commands t)))

;;; `magic-latex-buffer':
;; Literally magic. This makes buffers really nice.
(use-package magic-latex-buffer
  :commands magic-latex-buffer
  :delight magic-latex-buffer
  :ghook ('LaTeX-mode-hook #'magic-latex-buffer)
  :init
  (progn
    (csetq magic-latex-enable-block-highlight t ;; Prettify blocks that change their font size
           magic-latex-enable-suscript t        ;; Prettify sub and super script blocks
           magic-latex-enable-pretty-symbols t  ;; Convert latex variables into their UTF8 symbol
           magic-latex-enable-block-align nil   ;; Don't make \centering blocks appear centered in the LaTeX buffer
           magic-latex-enable-inline-image t))) ;; Display images inline in the LaTeX document

;;; `latex-extra':
;; This package adds a bunch of small but nice quality of life enhancements to
;; `auctex', mostly for folding content, moving quickly between sections and
;; better handling of `auto-fill-mode'.
(use-package latex-extra
  :delight
  ;; This fucking thing just wont shut up
  :commands latex-extra-mode
  :functions latex-extra-mode
  :ghook ('LaTeX-mode-hook #'latex-extra-mode))

;;; `reftex':
;; This is a specialized package for labels, references and citations. It is
;; really awesome, and is such a massive help when writing documents that
;; requires references etc.
(use-package reftex
  :delight
  :commands (turn-on-reftex reftex-mode)
  :ghook ('LaTeX-mode-hook #'turn-on-reftex)
  :init
  (progn
    (csetq reftex-plug-into-AUCTeX t                                       ;; Enable the integration with `auctex'
           reftex-use-fonts t                                              ;; Prettify things
           reftex-default-bibliography '("~/Code/UiB/bibliography.bib")))) ;; Default location of references

;;; `company-reftex':
;; Enable auto completion for reftex in Company.
(use-package company-reftex
  :config
  (progn
    (add-to-list 'company-backends 'company-reftex-labels t)
    (add-to-list 'company-backends 'company-reftex-citations t)))

;;; `ivy-bibtex':
;; This package is really useful for working with bibliographies, its primary
;; usage is automatic generation of the key for entries and for quickly
;; inserting them as well.
(use-package ivy-bibtex
  :init
  (progn
    (csetq ivy-re-builders-alist ;; Required for using this package
           '((ivy-bibtex . ivy--regex-ignore-order)
             (t . ivy--regex-plus))))
  :config
  (progn
    (csetq bibtex-dialect 'biblatex     ;; Use a new and "modern" BibTeX format
           bibtex-align-at-equal-sign t ;; Align entries in our `.bib' file at `='

           ;; Configuration for how to format keys for bibliography entries, I've
           ;; changed this to be like how `zotero' does it. I can't remember how
           ;; the default looks, but with how this is configured they keys will
           ;; look like this: `munroe2015PublicKey'. Author name first, then year
           ;; and then name of paper/book etc.
           bibtex-autokey-year-length 4                                       ;; Use full years
           bibtex-autokey-name-year-separator ""                              ;; Don't separate the year and author
           bibtex-autokey-year-title-separator ""                             ;; Or the year and title
           bibtex-autokey-titleword-separator ""                              ;; Or the words in the title
           bibtex-autokey-titlewords 4                                        ;; The key should be four words
           bibtex-autokey-titlewords-stretch 2                                ;; With two extra words from the title
           bibtex-autokey-titleword-length t                                  ;; Use all characters from title
           bibtex-autokey-titleword-case-convert-function 'identity           ;; Preserve casing on title
           ivy-bibtex-default-action 'bibtex-completion-insert-citation       ;; Automatically insert citation
           bibtex-completion-bibliography '("~/Code/UiB/bibliography.bib")))) ;; Default location of bibliography

;;; `ebib':
;; On the other side of the same coin, `ebib' makes managing and editing your
;; bibliography amazingly easy. It builds on top of the previous configuration,
;; but gives you a full mode wherein you can change, update and fix bibliography
;; entries. It's amazing.
(use-package ebib
  :config
  (progn
    (csetq ebib-bibtex-dialect 'biblatex                              ;; Use same dialect as BibTeX
           ebib-preload-bib-files '("~/Code/UiB/bibliography.bib")))) ;; Default location of bibliography

;;;;;; Markdown
(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (csetq markdown-command "pandoc --from=markdown --to=html --standalone --mathjax" ;; Use pandoc to convert documents from markdown to HTML
         markdown-enable-wiki-links t                                               ;; Syntax highlighting for wiki links
         markdown-italic-underscore t                                               ;; Use underscores for italic text
         markdown-make-gfm-checkboxes-buttons t                                     ;; Make checkboxes into buttons you can interact with
         markdown-gfm-additional-languages '("sh")                                  ;; Add `sh' as a language to convert
         markdown-fontify-code-blocks-natively t))                                  ;; Highlight code using the languages major mode

;;;;; Org
;;;;;; Core configuration
;; Use org-mode, it's awesome.

;;; `org':
;; Org-mode is an amazing piece of work, it can more or less do everything that
;; you can think of, spread sheets, interactive coding, notes, exporting to
;; everything under the sun and so on
(use-package org
  :defines (org-export-with-sub-superscripts org-babel-do-load-languages)
  :commands org-babel-do-load-languages
  :config
  (progn
    (csetq org-src-fontify-natively t                       ;; Always use syntax highlighting of code blocks
           org-startup-with-inline-images t                 ;; Always show images
           org-startup-indented t                           ;; Indent text according to the current header
           org-hide-emphasis-markers t                      ;; Hides the symbols that makes text bold, italics etc
           org-use-sub-superscripts '{}                     ;; Always use {} to group sub/superscript text
           org-export-with-sub-superscripts '{}             ;; Export with the same syntax as above
           org-preview-latex-default-process 'dvisvgm       ;; Use dvisvgm for better quality LaTeX fragments
           org-format-latex-options
           (plist-put org-format-latex-options :scale 1.25) ;; Make the preview a little larger
           org-catch-invisible-edits 'smart                 ;; Smart editing of hidden regions
           org-highlight-latex-and-related '(latex)         ;; Highlight LaTeX fragments, snippets etc
           org-pretty-entities t                            ;; Show entities as UTF8-characters when possible
           org-list-allow-alphabetical t                    ;; Allow lists to be a) etc
           org-confirm-babel-evaluate nil                   ;; Don't bug about executing code all the time
           org-babel-python-command "python3")              ;; Newer is always better

    ;; Configure which languages we can use in Org Babel code blocks
    ;; NOTE: This slows down the startup of Org-mode a little bit
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((shell . t)
       (emacs-lisp . t)
       (dot . t)
       (latex . t)
       (python .t)
       (java . t)))))

;; I don't want the mode line to show that org-indent-mode is active
(use-package org-indent :after org :delight)

(use-package org-ref
  :init
  (progn
    (csetq reftex-default-bibliography '("~/Code/UiB/bibliography.bib")
           org-ref-completion-library 'org-ref-ivy-bibtex)))

;;;;;; Agenda
(use-package org-agenda
  :after org
  :init (add-to-list 'org-modules 'org-habit t)
  :config
  (progn
    (csetq org-agenda-files '("~/.org/routine.org"
                              "~/.org/school.org"
                              "~/.org/work.org"
                              "~/.org/workouts.org"))))

(use-package org-super-agenda
  :after org)

;;;;;; Bindings
;;; vim-like confirm/abort for capture and src
;;; Taken from mwillsey (Max Willsey) on https://github.com/syl20bnr/spacemacs/pull/7400
(with-eval-after-load 'org-capture
  (define-key org-capture-mode-map [remap evil-save-and-close]          'org-capture-finalize)
  (define-key org-capture-mode-map [remap evil-save-modified-and-close] 'org-capture-finalize)
  (define-key org-capture-mode-map [remap evil-quit]                    'org-capture-kill))

(with-eval-after-load 'org-src
  (define-key org-src-mode-map [remap evil-save-and-close]              'org-edit-src-exit)
  (define-key org-src-mode-map [remap evil-save-modified-and-close]     'org-edit-src-exit)
  (define-key org-src-mode-map [remap evil-quit]                        'org-edit-src-abort))

(with-eval-after-load 'org-table
  (define-key org-table-fedit-map [remap evil-save-and-close]           'org-table-fedit-finish)
  (define-key org-table-fedit-map [remap evil-save-modified-and-close]  'org-table-fedit-finish)
  (define-key org-table-fedit-map [remap evil-quit]                     'org-table-fedit-abort))

;;;;;; Capture
;; Capturing of todos and fixmes and so on in projects and in regular life. Uses
;; `counsel-projectile-org-capture' to automatically put them in their correct
;; projects and if not puts them in my inbox.

(defcustom amalthea-capture-projects "~/.org/projects.org"
  "Inbox file for project related tasks."
  :type 'string
  :group 'amalthea)

(defcustom amalthea-capture-inbox "~/.org/inbox.org"
  "Inbox for personal tasks, reminders and so on."
  :type 'string
  :group 'amalthea)

(defcustom amalthea-capture-notes "~/.org/notes.org"
  "Location of notes file for assorted things that I want to remark on."
  :type 'string
  :group 'amalthea)

(use-package org-capture
  :ghook ('org-capture-mode-hook #'evil-insert-state)
  :general
  (amalthea-leader "c" '(counsel-projectile-org-capture :wk "capture"))
  :init
  (progn
    (csetq org-capture-templates '(("t" "Personal TODO" entry
                                    (file+headline amalthea-capture-inbox "Inbox")
                                    "* TODO %?\n  %u\n  %a")
                                   ("n" "Personal note" entry
                                    (file+headline amalthea-capture-notes "Inbox")
                                    "* %?\n  %u\n  %a"))
           counsel-projectile-org-capture-templates '(("pt" "[${name}] TODO" entry
                                                       (file+headline amalthea-capture-projects "Inbox")
                                                       "* TODO %?\n  %u\n  %a")
                                                      ("pf" "[${name}] FIXME" entry
                                                       (file+headline amalthea-capture-projects "Inbox")
                                                       "* FIXME %?\n  %u\n  %a")))))

;;;;;; Beamer
;; We need our own class for Beamer because the builtin one is ugly and doesn't
;; support LuaLaTeX, we just need to make sure not to include any of the default
;; packages.
(use-package ox-latex
  :config
  (add-to-list 'org-latex-classes
               '("beamer"
                 "\\documentclass[presentation]{beamer}
                \\usepackage[AUTO]{polyglossia}
                \\usepackage{fontspec}
                \\usepackage{microtype}
                \\usepackage{tabularx}
                \\usepackage{booktabs}
                \\usepackage{listings}
                \\usepackage{pgfpages}
                \\setbeameroption{show notes on second screen=right}
                [NO-DEFAULT-PACKAGES]
                [NO-PACKAGES]
                [EXTRA]"
		             ("\\section{%s}" . "\\section*{%s}")
		             ("\\subsection{%s}" . "\\subsection*{%s}")
		             ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
               t))

(use-package ox-beamer
  :config
  (csetq org-beamer-theme "metropolis" ;; Use the `metropolis' theme
         org-beamer-frame-level 2))    ;; Give the slides some more depth

;;;;;; LaTeX
;;; `org-latex'
;; Org-mode has some really amazing exporting options, LaTeX included, but I
;; find the default configuration fairly lacking, so we'll add a bunch of
;; changes and add a custom LaTeX class.
(use-package ox-latex
  :config
  (csetq org-latex-classes
         '(("memoir-book"
            "\\documentclass[12pt,a4paper,oneside]{memoir}
            [DEFAULT-PACKAGES]

            \\defaultfontfeatures{Ligatures=TeX}
            \\newfontfeature{Microtype}{protrusion=default;expansion=default}
            \\setmainfont{Linux Libertine O}
            \\setsansfont{Linux Biolinum O}
            \\setmonofont{DejaVu Sans Mono}[Scale=MatchLowercase]

            [PACKAGES]

            \\addbibresource{/Users/sondre/Code/UiB/bibliography.bib}
            \\chapterstyle{veelo}
            \\headstyles{memman}
            \\pagestyle{ruled}

            [EXTRA]"
            ("\\chapter{%s}" . "\\chapter*{%s}")
            ("\\section{%s}" . "\\section*{%s}")
            ("\\subsection{%s}" . "\\subsection*{%s}")
            ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
            ("\\paragraph{%s}" . "\\paragraph*{%s}")
            ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
           ("memoir"
            "\\documentclass[12pt,a4paper,oneside]{memoir}
            [DEFAULT-PACKAGES]

            \\defaultfontfeatures{Ligatures=TeX}
            \\newfontfeature{Microtype}{protrusion=default;expansion=default}
            \\setmainfont{Linux Libertine O}
            \\setsansfont{Linux Biolinum O}
            \\setmonofont{DejaVu Sans Mono}[Scale=MatchLowercase]

            [PACKAGES]

            \\addbibresource{/Users/sondre/Code/UiB/bibliography.bib}
            \\counterwithin{table}{section}
            \\numberwithin{equation}{chapter}
            \\counterwithin{figure}{section}
            \\setenumerate[0]{label= (\\alph*)}
            \\AtBeginDocument{\\counterwithin{lstlisting}{section}}
            \\counterwithout{section}{chapter}
            \\settocdepth{subsection}
            \\setsecnumdepth{subsection}
            \\headstyles{memman}
            \\pagestyle{ruled}

            [EXTRA]"
            ("\\section{%s}" . "\\section*{%s}")
            ("\\subsection{%s}" . "\\subsection*{%s}")
            ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
            ("\\paragraph{%s}" . "\\paragraph*{%s}")
            ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))
  (csetq org-format-latex-header
         "\\documentclass[12pt,a4paper,oneside]{memoir}
           [DEFAULT-PACKAGES]
           [NO-PACKAGES]
           \\defaultfontfeatures{Ligatures=TeX}
           \\newfontfeature{Microtype}{protrusion=default;expansion=default}
           \\setmainfont{Linux Libertine O}
           \\setsansfont{Linux Biolinum O}
           \\setmonofont{DejaVu Sans Mono}[Scale=MatchLowercase]
           \\pagestyle{empty}             % do not remove
           % The settings below are copied from fullpage.sty
           \\setlength{\\textwidth}{\\paperwidth}
           \\addtolength{\\textwidth}{-3cm}
           \\setlength{\\oddsidemargin}{1.5cm}
           \\addtolength{\\oddsidemargin}{-2.54cm}
           \\setlength{\\evensidemargin}{\\oddsidemargin}
           \\setlength{\\textheight}{\\paperheight}
           \\addtolength{\\textheight}{-\\headheight}
           \\addtolength{\\textheight}{-\\headsep}
           \\addtolength{\\textheight}{-\\footskip}
           \\addtolength{\\textheight}{-3cm}
           \\setlength{\\topmargin}{1.5cm}
           \\addtolength{\\topmargin}{-2.54cm}")
  (csetq org-latex-compiler "lualatex"                       ;; Use a modern LaTeX compiler
         org-latex-default-class "memoir"                    ;; Use my own class by default
         org-latex-default-table-environment "tabularx"      ;; Use a better table formatter
         org-latex-tables-booktabs t                         ;; Always use booktabs for better looking tables
         org-latex-prefer-user-labels t                      ;; Prefer labels I make myself please
         org-latex-listings t                                ;; Make SRC blocks export to code blocks in LaTeX
         org-export-with-smart-quotes t                      ;; Export quotes smartly
         org-latex-pdf-process
         (list "latexmk -pvc- %f -cd %o")                    ;; Use `latexmk' to generate PDF
         org-latex-listings-options                          ;; Configure source code exporting
         '(("frame" "tb")                                    ;; Single lines at the top and bottom of frame
           ("columns" "fullflexible")                        ;; Fix spacing in source code
           ("flexiblecolumns" "true")                        ;; Same as above
           ("numbers" "left")                                ;; Show line numbers on the left
           ("numberstyle" "\\ttfamily\\color{gray}\\tiny")   ;; Monospaced gray tiny line numbers
           ("showstringspaces" "false")                      ;; Don't show spaces in strings as underlines
           ("basicstyle" "\\ttfamily\\footnotesize"))        ;; Use footnote sized monospace font
         org-latex-default-packages-alist                    ;; Configure default packages inserted into LaTeX classes
         '(("AUTO" "polyglossia" t)                          ;; Polyglossia for language settings, automatically configured
           ("" "fontspec" t)                                 ;; Fancy fonts for OpenType fonts in LuaLaTeX
           ("" "microtype" t)                                ;; Micro-typography, for when you need even more typography
           ("" "geometry" t)                                 ;; Enable configuring the geometry of the pages
           ("" "subfiles" t)                                 ;; Enables splitting up large .tex files into smaller parts
           ("" "float" t)                                    ;; Float environments in LaTeX
           ("font=small,labelfont=bf,format=hang" "caption") ;; Make the font in captions smaller
           ("" "amsfonts" t)                                 ;; Math fonts
           ("" "amssymb" t)                                  ;; Math symbols
           ("" "mathtools" t)                                ;; Extra math tools
           ("shortlabels" "enumitem" t)                      ;; Enumerate environment with an option to change numbering quickly
           ("" "multirow" t)                                 ;; Lines that span multiple columns etc in tables
           ("" "tabularx" t)                                 ;; A better table environment
           ("" "hyperref" t)                                 ;; Links inside the generated PDFs
           ("" "tikz" t)                                     ;; Awesome technical diagrams and everything in between
           ("edges" "forest" t)                              ;; Quick and really easy way to draw graphs
           ("" "graphicx" t)                                 ;; Embed graphics in LaTeX documents
           ("" "xcolor" t)                                   ;; Color utility for text etc
           ("" "colortbl" t)                                 ;; Color rows and columns in tables
           ("" "array" t)                                    ;; Arrays, like tables, but not
           ("" "listings" t))                                ;; Display source code in LaTeX-documents
         org-latex-packages-alist                            ;; Extra packages that we load after the default ones
         '(("autostyle,strict,autopunct" "csquotes" t)       ;; Quoting and citing made easy
           ("style=ieee,backend=biber" "biblatex" t))        ;; Bibliography and citing
         org-latex-hyperref-template "\\hypersetup{\n colorlinks=true,\n pdfauthor={%a},\n pdftitle={%t},\n pdfkeywords={%k},\n pdfsubject={%d},\n pdfcreator={%c},\n pdflang={%L}}\n")

  ;; Add Java to the list of languages for listings
  (add-to-list 'org-latex-listings-langs '(java "Java") t))

;;; `by-backend':
(defmacro by-backend (&rest body)
  "Change output depending on which exporter is used.

For example, if BODY is `latex', then export it as LaTeX code but
if BODY is `html' then convert it to a PNG to allow easy
embedding in web pages."
  `(case org-export-current-backend ,@body))

;;;;; Utilities
;;;;;; PragmataPro
(csetq prettify-symbols-unprettify-at-point 'right-edge)

(defconst pragmatapro-prettify-symbols-alist
  (mapcar (lambda (s)
            `(,(car s)
              .
              ,(vconcat
                (apply 'vconcat
                       (make-list
                        (- (length (car s)) 1)
                        (vector (decode-char 'ucs #X0020) '(Br . Bl))))
                (vector (decode-char 'ucs (cadr s))))))
          '(("[ERROR]"   #XE380)
            ("[DEBUG]"   #XE381)
            ("[INFO]"    #XE382)
            ("[WARN]"    #XE383)
            ("[WARNING]" #XE384)
            ("[ERR]"     #XE385)
            ("[FATAL]"   #XE386)
            ("[TRACE]"   #XE387)
            ("[FIXME]"   #XE388)
            ("[TODO]"    #XE389)
            ("[BUG]"     #XE38A)
            ("[NOTE]"    #XE38B)
            ("[HACK]"    #XE38C)
            ("[MARK]"    #XE38D)
            ("!!"        #XE900)
            ("!="        #XE901)
            ("!=="       #XE902)
            ("!!!"       #XE903)
            ("!≡"        #XE904)
            ("!≡≡"       #XE905)
            ("!>"        #XE906)
            ("!=<"       #XE907)
            ("#("        #XE920)
            ("#_"        #XE921)
            ("#{"        #XE922)
            ("#?"        #XE923)
            ("#>"        #XE924)
            ("##"        #XE925)
            ("#_("       #XE926)
            ("%="        #XE930)
            ("%>"        #XE931)
            ("%>%"       #XE932)
            ("%<%"       #XE933)
            ("&%"        #XE940)
            ("&&"        #XE941)
            ("&*"        #XE942)
            ("&+"        #XE943)
            ("&-"        #XE944)
            ("&/"        #XE945)
            ("&="        #XE946)
            ("&&&"       #XE947)
            ("&>"        #XE948)
            ("$>"        #XE955)
            ("***"       #XE960)
            ("*="        #XE961)
            ("*/"        #XE962)
            ("*>"        #XE963)
            ("++"        #XE970)
            ("+++"       #XE971)
            ("+="        #XE972)
            ("+>"        #XE973)
            ("++="       #XE974)
            ("--"        #XE980)
            ("-<"        #XE981)
            ("-<<"       #XE982)
            ("-="        #XE983)
            ("->"        #XE984)
            ("->>"       #XE985)
            ("---"       #XE986)
            ("-->"       #XE987)
            ("-+-"       #XE988)
            ("-\\/"      #XE989)
            ("-|>"       #XE98A)
            ("-<|"       #XE98B)
            (".."        #XE990)
            ("..."       #XE991)
            ("..<"       #XE992)
            (".>"        #XE993)
            (".~"        #XE994)
            (".="        #XE995)
            ("/*"        #XE9A0)
            ("//"        #XE9A1)
            ("/>"        #XE9A2)
            ("/="        #XE9A3)
            ("/=="       #XE9A4)
            ("///"       #XE9A5)
            ("/**"       #XE9A6)
            (":::"       #XE9AF)
            ("::"        #XE9B0)
            (":="        #XE9B1)
            (":≡"        #XE9B2)
            (":>"        #XE9B3)
            (":=>"       #XE9B4)
            (":("        #XE9B5)
            (":-("       #XE9B6)
            (":)"        #XE9B7)
            (":-)"       #XE9B8)
            (":/"        #XE9B9)
            (":\\"       #XE9BA)
            (":3"        #XE9BB)
            (":D"        #XE9BC)
            (":P"        #XE9BD)
            (":>:"       #XE9BE)
            (":<:"       #XE9BF)
            ("<$>"       #XE9C0)
            ("<*"        #XE9C1)
            ("<*>"       #XE9C2)
            ("<+>"       #XE9C3)
            ("<-"        #XE9C4)
            ("<<"        #XE9C5)
            ("<<<"       #XE9C6)
            ("<<="       #XE9C7)
            ("<="        #XE9C8)
            ("<=>"       #XE9C9)
            ("<>"        #XE9CA)
            ("<|>"       #XE9CB)
            ("<<-"       #XE9CC)
            ("<|"        #XE9CD)
            ("<=<"       #XE9CE)
            ("<~"        #XE9CF)
            ("<~~"       #XE9D0)
            ("<<~"       #XE9D1)
            ("<$"        #XE9D2)
            ("<+"        #XE9D3)
            ("<!>"       #XE9D4)
            ("<@>"       #XE9D5)
            ("<#>"       #XE9D6)
            ("<%>"       #XE9D7)
            ("<^>"       #XE9D8)
            ("<&>"       #XE9D9)
            ("<?>"       #XE9DA)
            ("<.>"       #XE9DB)
            ("</>"       #XE9DC)
            ("<\\>"      #XE9DD)
            ("<\">"      #XE9DE)
            ("<:>"       #XE9DF)
            ("<~>"       #XE9E0)
            ("<**>"      #XE9E1)
            ("<<^"       #XE9E2)
            ("<!"        #XE9E3)
            ("<@"        #XE9E4)
            ("<#"        #XE9E5)
            ("<%"        #XE9E6)
            ("<^"        #XE9E7)
            ("<&"        #XE9E8)
            ("<?"        #XE9E9)
            ("<."        #XE9EA)
            ("</"        #XE9EB)
            ("<\\"       #XE9EC)
            ("<\""       #XE9ED)
            ("<:"        #XE9EE)
            ("<->"       #XE9EF)
            ("<!--"      #XE9F0)
            ("<--"       #XE9F1)
            ("<~<"       #XE9F2)
            ("<==>"      #XE9F3)
            ("<|-"       #XE9F4)
            ("<<|"       #XE9F5)
            ("<-<"       #XE9F7)
            ("<-->"      #XE9F8)
            ("<<=="      #XE9F9)
            ("<=="       #XE9FA)
            ("==<"       #XEA00)
            ("=="        #XEA01)
            ("==="       #XEA02)
            ("==>"       #XEA03)
            ("=>"        #XEA04)
            ("=~"        #XEA05)
            ("=>>"       #XEA06)
            ("=/="       #XEA07)
            ("=~="       #XEA08)
            ("==>>"      #XEA09)
            ("≡≡"        #XEA10)
            ("≡≡≡"       #XEA11)
            ("≡:≡"       #XEA12)
            (">-"        #XEA20)
            (">="        #XEA21)
            (">>"        #XEA22)
            (">>-"       #XEA23)
            (">=="       #XEA24)
            (">>>"       #XEA25)
            (">=>"       #XEA26)
            (">>^"       #XEA27)
            (">>|"       #XEA28)
            (">!="       #XEA29)
            (">->"       #XEA2A)
            ("??"        #XEA40)
            ("?~"        #XEA41)
            ("?="        #XEA42)
            ("?>"        #XEA43)
            ("???"       #XEA44)
            ("?."        #XEA45)
            ("^="        #XEA48)
            ("^."        #XEA49)
            ("^?"        #XEA4A)
            ("^.."       #XEA4B)
            ("^<<"       #XEA4C)
            ("^>>"       #XEA4D)
            ("^>"        #XEA4E)
            ("\\\\"      #XEA50)
            ("\\>"       #XEA51)
            ("\\/-"      #XEA52)
            ("@>"        #XEA57)
            ("|="        #XEA60)
            ("||"        #XEA61)
            ("|>"        #XEA62)
            ("|||"       #XEA63)
            ("|+|"       #XEA64)
            ("|->"       #XEA65)
            ("|-->"      #XEA66)
            ("|=>"       #XEA67)
            ("|==>"      #XEA68)
            ("|>-"       #XEA69)
            ("|<<"       #XEA6A)
            ("||>"       #XEA6B)
            ("|>>"       #XEA6C)
            ("|-"        #XEA6D)
            ("||-"       #XEA6E)
            ("~="        #XEA70)
            ("~>"        #XEA71)
            ("~~>"       #XEA72)
            ("~>>"       #XEA73)
            ("[["        #XEA80)
            ("]]"        #XEA81)
            ("\">"       #XEA90)
            ("_|_"       #XEA97))))

(defun add-pragmatapro-prettify-symbols-alist ()
  "Push PragmataPro characters into `prettify-symbols-alist'."
  (dolist (alias pragmatapro-prettify-symbols-alist)
    (push alias prettify-symbols-alist)))

;;; Add the ligatures to all programming modes
(general-add-hook '(prog-mode-hook intero-repl-mode-hook)
                  #'add-pragmatapro-prettify-symbols-alist)

;;; `prettify':
;; Enables us to use ligatures in Emacs. It's awesome.
(global-prettify-symbols-mode t)

;; And once all is said and done, start emacsclient in the background so that we
;; can connect to it from other windows or the terminal.
(require 'server)
(unless (server-running-p)
  (server-start))

;; (provide 'init)
;;; init.el ends here
