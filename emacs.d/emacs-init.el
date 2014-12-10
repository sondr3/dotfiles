
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

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
 )

(setq-default
 tab-width 4
 indent-tabs-mode nil                  ;; indent with spaces and not tabs
 fill-column 80
 )

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(setq inhibit-startup-message t)
