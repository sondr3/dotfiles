;;; HELLO WORLD

;; Add a comment so emacs doesn't automatically add it
;; (package-initialize)

;; Set some paths
(setq core-dir (expand-file-name "core" user-emacs-directory))
(setq modules-dir (expand-file-name "modules" user-emacs-directory))
(setq bundles-dir (expand-file-name "bundles" user-emacs-directory))

(add-to-list 'load-path core-dir)
(add-to-list 'load-path modules-dir)
(add-to-list 'load-path bundles-dir)

;; And then we start the fun
;; Initially we need to import the core functionality

(require 'packages)
(require 'sane-defaults)

;; And then the appearance
(require 'appearance)

;; And then EVIL and keybinds
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("40bc0ac47a9bd5b8db7304f8ef628d71e2798135935eb450483db0dbbfff8b11" default)))
 '(package-selected-packages (quote (tao-theme uniquify use-package auto-compile))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
