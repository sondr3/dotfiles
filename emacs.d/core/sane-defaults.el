;; sane-defaults.el --- Because Emacs is old and quirky

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
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Save where you left off in buffers
(require 'saveplace)
(setq-default save-place t)

;; Show matching parenthesis
(show-paren-mode 1)

;; Do not use tabs!
(setq-default indent-tabs-mode nil)

;; And assorted commands stolen from 'better-defaults.el'
(setq x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t
      require-final-newline t
      visible-bell t
      load-prefer-newer t
      editt-window-setup-function 'ediff-setup-window-plain
      save-place-file (concat user-emacs-directory "places")
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))

(provide 'sane-defaults)
