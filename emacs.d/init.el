
(setq user-full-name "Sondre Nilsen"
      user-mail-addres "nilsen.sondre@gmail.com")

(when window-system
  (tooltip-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode 1)
  (scroll-bar-mode -1))

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/")
             '("marmalade" . "https://marmalade-repo.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

(use-package auto-compile
  :ensure t
  :config (auto-compile-on-load-mode))
(setq load-prefer-newer t)

(use-package evil
  :ensure t
  :init
  :config
  (evil 1))

(defun my/tangle-init()
  "If the current file is 'init.org', the code blocks are tangled and compiled"
  (when (equal (buffer-file-name)
               (expand-file-name (concat user-emacs-directory "init.org")))
    (let ((prog-mode-hook nil))
      (org-babel-tangle)
      (byte-compile-file (concat user-emacs-directory "init.el")))))

(add-hook 'after-save-hook 'my/tangle-init)
