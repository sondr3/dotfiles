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

;; Make Emacs save custom settings into a separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; And then we start the fun
;; Initially we need to import the core functionality

(require 'packages)
(require 'sane-defaults)

;; And then the appearance
(require 'appearance)

;; And then EVIL and keybinds
