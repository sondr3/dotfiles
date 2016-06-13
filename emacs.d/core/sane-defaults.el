;;; SANE DEFAULTS
;; Emacs is a pretty old editor and as such has a lof of quirks and
;; weirdnesses that I don't really like, so I'll be changing a bunch
;; of settings into what I consider to be sane defaults.

;; Save custom settings into custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

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

(provide 'sane-defaults)
