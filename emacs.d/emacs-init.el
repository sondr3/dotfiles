
(require 'req-package)
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(req-package exec-path-from-shell
  :init
  (when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)))

(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

(setq
 x-select-enable-clipboard t           ;; allow pasting outside Emacs
 global-auto-revert-non-file-buffers t ;; auto refresh dired quietly
 auto-revert-verbose nil
 echo-keystrokes 0.1                   ;; show keystrokes in progress
 delete-by-moving-to-trash t           ;; does what it says
 jump-char-lazy-highlight-face nil     ;; don't highlight matches with jump-char
 line-number-mode t                    ;; display line numbers
 column-number-mode t                  ;; and columns
 fill-column 80                        ;; lines should be 80 chars wide
 gc-cons-threshold 20000000            ;; we have tons of memory now, don't be greedy
 enable-recursive-minibuffers t        ;; let minibuffers be recursive
 visible-bell t                        ;; hide that ding ding
 font-lock-maximum-decoration t        ;; decorate all buffers with colors
 color-theme-is-global t               ;; does what it says
 truncate-partial-width-windows nil    ;; truncate buffers
 tab-always-indent 'complete           ;; tab before indenting
 require-final-newline t               ;; always add a newline when saving
 show-paren-delay 0                    ;; because saving miliseconds is woreth it
 load-prefer-newer t                   ;; prefer newer .el files to .elc
 )

(setq-default
 tab-width 4
 indent-tabs-mode nil                  ;; indent with spaces and not tabs
 fill-column 80                        ;; max 80 characters wide
 auto-fill-function 'do-auto-fill      ;; automatically wrap long lines
 )

(show-paren-mode t)    ;; show matching paranthesises
(blink-cursor-mode -1) ;; don't blink the cursor

(require 'ido)
(ido-mode t)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'erase-buffer 'disable 'nil)

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(setq inhibit-startup-message t)

(set-default-font "Monaco")
(set-face-attribute 'default nil
                    :family "Monaco"
                    :height 120
                    :weight 'normal
                    :width 'normal)

(load-theme 'zenburn t)

(req-package evil
  :require (undo-tree surround helm-config ace-jump-mode)
  :ensure evil
  :init
  (progn
    (setq evil-default-cursor t)
    (evil-mode 1)
    (setq evil-motion-state-modes
          (append evil-emacs-state-modes evil-motion-state-modes))))

(req-package evil-leader
  :require evil
  :ensure evil-leader
  :init
  (progn
    (evil-leader/set-leader "<SPC>")
    (global-evil-leader-mode 1)))

(req-package evil-surround
  :require evil
  :ensure evil-surround
  :init
  (global-surround-mode 1))

(req-package undo-tree
  :diminish ""
  :init
  (progn
    (setq undo-tree-auto-save-history t)
    (global-undo-tree-mode)))

(req-package ace-jump-mode)

(req-package-finish)
