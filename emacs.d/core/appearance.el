;;; Appearance
;;
;; Contains all my settings that will change how Emacs looks

;; First, we need to set a proper font for the buffers
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

;; Then we can set the theme, I've currently settled on Tao but will probably
;; create my own because I'm like that

(use-package tao-theme)


(provide 'appearance)
