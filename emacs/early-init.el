;; -*- lexical-binding: t -*-

;;; Commentary:

;; The currently unreleased version of Emacs (version 27) introduces yet another
;; init file, this one called `early-init.el', which runs before `package.el'
;; and the interface is initialized. This is useful for optimizing the boot
;; process of Emacs even more.

;; What we do are some fairly well known optimizations, we disable the file name
;; handler as it is called on every `require' and `load', so this will
;; marginally speed up the boot. Then we change the the garbage collectors
;; values so it doesn't start collecting too early. We reset both of these
;; values once Emacs has started. Last, but not least, we tell Emacs not to
;; worry about `package.el'.

;; Finally we disable a few bars that we don't need since we can now do this
;; before they are initialized, saving us a tiny flash of them when we boot up.

;;; Code:

;; Silence byte compiler warnings
(defvar amalthea--file-name-handler-alist file-name-handler-alist)
;; Straight up stolen from Doom Emacs' hints on speeding up booting
(setq gc-cons-threshold 402653184
      gc-cons-percentage 1.0
      file-name-handler-alist nil)

(defun amalthea--early-init ()
  "Reset the values we changed before boot."
  (setq file-name-handler-alist amalthea--file-name-handler-alist
        gc-cons-threshold 16777216
        gc-cons-percentage 0.15))

;; Reset the values we stole from Doom back to normal
(add-hook 'emacs-startup-hook #'amalthea--early-init)

;; Initialize packaging, required for how we use `Nix'.
(require 'package)
(setq load-prefer-newer t            ;; Always load the newest file between `.el' and `.elc'
      package--init-file-ensured t   ;; We do initialize our packages, yes
      package-archives nil           ;; But we do not use `package.el' for installation, so disable it
      package-enable-at-startup nil) ;; Don't enable installed packages on boot
(package-initialize)

;; `no-littering':
;; Yes, we load it this early. And that is purely because the function that byte
;; compiles Amalthea is really stupid, so we load this before doing anything.
(require 'no-littering)

;; HIDE ALL THE THINGS!
(if (fboundp 'menu-bar-mode)
    (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

;; (provide 'early-init)
;;; early-init.el ends here
