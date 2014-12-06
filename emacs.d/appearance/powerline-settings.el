(add-to-list 'load-path "~/.emacs.d/vendor/emacs-powerline")

(require 'powerline)

(setq powerline-arrow-shape 'arrow)

(setq powerline-color1 "#073642")
(setq powerline-color2 "#002b36")

(set-face-attribute 'mode-line nil
                    :foreground "#fdf6e3"
                    :background "#2aa198"
                    :box nil)
(set-face-attribute 'mode-line-inactive nil
                    :box nil)

(provide 'powerline-settings)
