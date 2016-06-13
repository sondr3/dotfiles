;; Add a few more repositories for packages
(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("org" . "http://orgmode.org/elpa/")
        ("melpa" . "http://melpa.org/packages/")))

;; But first, newer is always better
(setq load-prefer-newer t
      package-enable-at-startup nil)

;; And then we initialize
(package-initialize)

;; And now we make sure use-package is installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; ANd initialize it!
(eval-when-compile
  (require 'use-package))

;; Always ensure packages are installed
(setq use-package-always-ensure t
      use-package-always-defer t)

;; And finally, let's auto-compile emacs-lisp files
(use-package auto-compile
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

(provide 'packages)
