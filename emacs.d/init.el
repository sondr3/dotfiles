(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)

(require 'ob-tangle)
(org-babel-load-file
 (expand-file-name "emacs-init.org"
		               user-emacs-directory))
