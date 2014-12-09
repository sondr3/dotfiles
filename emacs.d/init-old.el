;; Turn off menues and stuff completely so it doens't momentarily flashes while loading
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Hide splash screen
(setq inhibit-startup-message t)

;; MELPA packages for package goodness
(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; Make the path on OSX properly initialize so you don't get funny errors
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; Load the different config and theme files
(add-to-list 'load-path "~/.emacs.d/appearance") ;; themes/fonts etc
(add-to-list 'load-path "~/.emacs.d/core") ;; core settings
(add-to-list 'load-path "~/.emacs.d/modules") ;; language specific settings
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes") ;; theme save path

;; Configure the looks of Emacs
(require 'appearance)
(require 'font)
(require 'theme)
;; (require 'powerline-settings)

;; Some core settings
(require 'core)
(require 'packages)
(require 'evil)
