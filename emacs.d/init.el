;; Turn off menues and stuff completely
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
(package-initialize) ;; You might already have this line

;; Make the path on OSX proper
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; Load the different config and theme files
(add-to-list 'load-path "~/.emacs.d/appearance")
(add-to-list 'load-path "~/.emacs.d/core")
(add-to-list 'load-path "~/.emacs.d/languages")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;; Configure the looks of Emacs
(require 'appearance)
(require 'font)
(require 'theme)
;; (require 'powerline-settings)

;; Some core settings
(require 'core)
