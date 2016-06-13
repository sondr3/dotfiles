;;;; HELLO WORLD

;;; PACKAGES
;; We'll be using the excellent use-package with auto-compile to
;; automatically handle, install and configure packages. It's
;; great.

;; Add a few more repositories for packages
(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("org" . "http://orgmode.org/elpa/")
        ("melpa" . "http://melpa.org/packages/")))

;; But first, newer is always better
(setq load-prefer-newer t
      package-enable-at-startup nil)

;; And then we initialize
(package-initialize)

;; And now we make sure use-package is installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; And initialize it!
(eval-when-compile
  (require 'use-package))

;; Always ensure packages are installed
(setq use-package-always-ensure t
      use-package-always-defer t)

;; And finally, let's auto-compile emacs-lisp files
(use-package auto-compile
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

;;; SANE DEFAULTS
;; Emacs is a pretty old editor and as such has a lof of quirks and
;; weirdnesses that I don't really like, so I'll be changing a bunch
;; of settings into what I consider to be sane defaults.

;; Disable the splash screen and startup messages
(setq inhibit-startup-message t
      initial-scratch-message "")
(defalias 'display-startup-echo-area-message #'ignore)

;; First we use IDO
(ido-mode t)
(setq ido-enable-flex-matching t)

;; Disable some bars
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

;; Show matching parenthesis
(show-paren-mode 1)

;; Do not use tabs, it's heresy
(setq-default indent-tabs-mode nil)

;; However, I do like to indent by 2, not 4
(setq-default tab-width 2)

;; 'y' or 'n' instead of 'yes' and 'no'
(defalias 'yes-or-no-p 'y-or-n-p)

;; UTF-8 ALL THE THINGS
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8
      locale-coding-system 'utf-8)

;; Sentences end in one space
(set-default 'sentence-end-double-space nil)

;; Set backup directories
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "saves")))
      auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory "auto-save") t)))

;; Backup settings
(setq backup-by-copying t
      version-control t
      delete-old-versions t)

;; Make buffer names unique
(use-package uniquify
  :ensure nil
  :config
  (progn
    (setq uniqify-buffer-name-style 'forward)))

;; Save where you left off in buffers
(use-package saveplace
  :ensure nil
  :init (save-place-mode 1)
  :config
  (progn
    (setq-default save-place t)))

;; Save a list of recent files
(use-package recentf
  :ensure nil
  :init (recentf-mode 1)
  :config
  (progn
    (setq recentf-max-saved-items 100)))

;; And the minibuffer as well
(use-package savehist
  :ensure nil
  :init (savehist-mode 1)
  :config
  (progn
    (setq history-length 1000)))

;; Copying and pasting
(setq select-enable-clipboard t
      select-enable-primary t
      save-interprogram-paste-before-kill t
      mouse-yank-at-point t)

;; And fuck that bell and its sound
(setq visible-bell nil
      ring-bell-function 'ignore)

;; Don't blink the cursor
(blink-cursor-mode -1)

;; Fix $PATH on OS X
(use-package exec-path-from-shell
  :config
  (progn
    (exec-path-from-shell-initialize)))

;; Always start maximized
(add-hook 'window-setup-hook 'toggle-frame-fullscreen t)

;;; APPEARANCE
;; Here I'll be setting how Emacs looks, very personal preference
;; stuff.

;; Always fontify text
(setq font-lock-maximum-decoration t)

;; Highlight the current line
(global-hl-line-mode 1)

;; Set a proper font for the buffers
(set-face-attribute 'default nil
                    :family "Source Code Pro"
                    :height 120
                    :weight 'normal)

;; Then one for the mode line
(set-face-attribute 'mode-line nil
                    :family "Source Code Pro"
                    :height 110
                    :weight 'light)

;; And have a bit of line spacing, it just looks good
(setq-default line-spacing 0.15)

;; Then we can configure the theme
(use-package tao-theme
  :config
  (load-theme 'tao-yang))

;; I also want a bit of a fringe
(fringe-mode '(16 . 16))

;; I want line numbers, but they should be relative
(use-package nlinum-relative
  :config
  (progn
    ;; (nlinum-relative-setup-evil)
    (setq nlinum-format " %3s "
          nlinum-relative-current-symbol ""
          nlinum-relative-redisplay-delay 0)
    (add-hook 'prog-mode-hook 'nlinum-relative-mode)
    (add-hook 'text'mode-hook 'nlinum-relative-mode)))
