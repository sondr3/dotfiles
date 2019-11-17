;;; am-ui.el --- Amalthea UI configuration -*- lexical-binding: t -*-

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
;; Configurations for making Emacs and Amalthea a better looking editor,
;; something that Emacs sorely needs, because it's pretty damn ugly by default.

;;; Code:
;;; Fonts
(set-face-attribute 'default nil
		                :family amalthea-mono-font
		                :height amalthea-font-size)
(set-face-attribute 'variable-pitch nil
		                :family amalthea-serif-font
		                :height amalthea-font-size)
(set-frame-font amalthea-mono-font nil t)

;;; General UI settings:
;; Starting with Emacs 26.1, Emacs has a built-in line number mode written in C
;; that supports relative line numbers, like in Vim. This is awesome, because
;; the previous offerings had a whole bunch of drawbacks, either being really
;; slow, or not properly supporting relative line numbers in Org mode and so on.
;; Thankfully we now have a proper solution.

;; First, we set the relative line number to `visual', which counts the visible
;; lines on the screen. Otherwise headers that are folded in Org makes the line
;; count go haywire, next we set a default width for line numbers and ensure
;; that it doesn't get narrower or wider depending on the amount of lines in a
;; file.

;; Finally, we disable the visible and audible bell that is enabled by default
;; because it's bloody annoying.
(setq-default line-spacing amalthea-line-spacing      ;; Give lines some breathing room
              frame-title-format '("Amalthea ∷ %b")  ;; Name the Emacs window
              display-line-numbers 'visual            ;; Count the visible line numbers, not the actual number
              display-line-numbers-current-absolute t ;; Show line number of current line instead of 0
              display-line-numbers-width 4            ;; Enough space for huge files
              display-line-numbers-widen nil          ;; Disable dynamic sizing of line number width
              visible-bell nil                        ;; No bells
              ring-bell-function #'ignore)            ;; NO BELLS

;;; `hl-line':
;; This is basically something that I learned to use and love from Vim, it
;; highlights the current line where the cursor is currently active. We enable
;; this minor mode globally, and then make it so it doesn't display in inactive
;; windows.
(use-package hl-line
  :commands global-hl-line-mode
  :init (global-hl-line-mode)
  :config (csetq global-hl-line-sticky-flag nil)) ;; Don't highlight current line in inactive buffers

;;; Window
;; Windows 10

(defhydra hydra-zoom (:color red :hint nil)
  "zoom"
  ("k" text-scale-increase "in")
  ("j" text-scale-decrease "out")
  ("r" (text-scale-adjust 0) "reset" :color blue)
  ("q" nil "quit" :color blue))

;;; Keybindings for windows
(amalthea-leader
  "w f" '(toggle-frame-fullscreen :wk "fill screen")
  "w m" '(toggle-frame-maximized :wk "maximize")
  "w z" '(hydra-zoom/body :wk "zoom"))

;;; Theme
;; You should probably change this, I have a very weird taste in themes.
(use-package apropospriate-theme
  :init (load-theme 'apropospriate-light t))

;; Fix the spacing between the major mode name and all the minor modes.
(catch 'done
  (mapc (lambda (x)
          (when (and (consp x)
                     (equal (cadr x) '("" minor-mode-alist)))
            (setcar (cadr x) " ∷ ")
            (throw 'done t)))
        mode-line-modes))


(provide 'am-ui)

;;; am-ui.el ends here
