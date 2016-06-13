;;; APPEARANCE
;; Here I'll be setting how Emacs looks, very personal preference
;; stuff. If it would only work...

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
  (load-theme 'tao-yang t))

;; I also want a bit of a fringe
(fringe-mode '(16 . 16))

;; I want line numbers, but they should be relative
(use-package nlinum-relative
  :init (global-nlinum-relative-mode)
  :config
  (progn
    ;; (nlinum-relative-setup-evil)
    (setq nlinum-format " %3s "
          nlinum-relative-current-symbol ""
          nlinum-relative-redisplay-delay 0)
    (add-hook 'prog-mode-hook 'nlinum-relative-mode)
    (add-hook 'text'mode-hook 'nlinum-relative-mode)))

(provide 'appearance)
