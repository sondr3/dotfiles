;;; early-init.el --- I heard you like init -*- lexical-binding: t -*-

;; Author: Sondre Nilsen
;; Maintainer: Sondre Nilsen
;; Version: 0.1
;; Package-Requires: ((emacs "27"))
;; Homepage: https://github.com/sondr3/amalthea

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License see
;; <http://www.gnu.org/licenses/>.

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

(defvar amalthea--file-name-handler-alist file-name-handler-alist)
(setq gc-cons-threshold 402653184
      gc-cons-percentage 1.0
      file-name-handler-alist nil)

(defun amalthea--early-init ()
  "Reset the values we changed before boot."
  (setq file-name-handler-alist amalthea--file-name-handler-alist
        gc-cons-threshold 16777216
        gc-cons-percentage 0.15))

(add-hook 'emacs-startup-hook #'amalthea--early-init)

(defvar package--init-file-ensured)
(setq user-emacs-directory (file-name-directory load-file-name)
      load-prefer-newer t
      package-enable-at-startup nil
      package--init-file-ensured t)

(if (fboundp 'menu-bar-mode)
    (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

;; (provide 'early-init)
;;; early-init.el ends here
