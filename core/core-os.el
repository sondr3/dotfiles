;;; core-os.el --- OS configuration -*- lexical-binding: t -*-

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Contains settings related to making Emacs work better on various operating
;; systems.

;; Linux: Configures and enables copying and pasting between Emacs and X11 and
;; choses the builtin tooltips over GTK.

;; macOS: Enables emojis to be properly rendered, makes it so the titlebar is
;; dark and not transparent and fixes a few related frame issues. Also fixes and
;; enables smoother scrolling for macOS.

;;; Code:

(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
(setq-am select-enable-clipboard t "Cut and paste from the actual clipboard")
(setq-am select-enable-primary t "Use the primary clipboard")

(when (eq system-type 'darwin)
  (set-fontset-font "fontset-default" 'unicode "Apple Color Emoji" nil 'prepend)
  (dolist (pair '((ns-transparent-titlebar . nil)
                  (ns-appearance . dark)))
    (push pair (alist-get 'ns window-system-default-frame-alist nil))
    (set-frame-parameter nil (car pair) (cdr pair)))

  (use-package exec-path-from-shell
    :commands exec-path-from-shell-initialize
    :config (exec-path-from-shell-initialize))

  (setq-am ns-use-thin-smoothing nil "Dont use thinner strokes on macOS")
  (setq-am mouse-wheel-flip-direction t "Change scrolling to new macOS defaults")
  (setq-am mouse-wheel-tilt-scroll t "Change scrolling to new macOS defaults"))

(when (eq system-type 'gnu/linux)
  (defvar x-gtk-use-system-tooltips nil)
  (setq-am x-gtk-use-system-tooltips nil "Use the builtin Emacs tooltips")
  (setq-am x-underline-at-descent-line t "Fix for not using GTK tooltips"))

(provide 'core-os)
;;; core-os.el ends here
