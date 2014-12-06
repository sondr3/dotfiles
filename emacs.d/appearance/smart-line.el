(require 'smart-mode-line)

(add-hook 'after-init-hook 'sml/setup)
(setq sml/no-confirm-load-theme t)

(sml/apply-theme 'powerline)

(provide 'smart-line)
