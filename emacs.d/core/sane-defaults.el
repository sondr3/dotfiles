;; sane-defaults.el --- Because Emacs is old and quirky

;; Disable the splash screen
(setq inhibit-startup-message t)

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

;; Make buffer names unique
(use-package uniquify
  :ensure nil
  :config
  (progn
    (setq uniqify-buffer-name-style 'forward)))

;; Save where you left off in buffers
(use-package saveplace
  :ensure nil
  :config
  (progn
    (setq-default save-place t)))

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

;; Save a list of recent files
(recentf-mode 1)
(setq recentf-max-saved-items 100)

;; And the minibuffer as well
(savehist-mode 1)
(setq history-length 1000)

;; Copying and pasting
(setq select-enable-clipboard t
      select-enable-primary t
      save-interprogram-paste-before-kill t
      mouse-yank-at-point t)

;; And fuck that bell and its sound
(setq visible-bell nil
      ring-bell-function 'ignore)

(provide 'sane-defaults)
