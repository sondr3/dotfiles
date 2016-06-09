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

(require 'sane-defaults)
(require 'packages)

;; And then the appearance
;; (require 'appearance)
(require 'fonts)

;; And then EVIL and keybinds
