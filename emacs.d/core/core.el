;; Save previous location in file
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

;; Change backup location
(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)

;; Backup files even when they are in git
(setq vc-make-backup-files t)

(provide 'core)
