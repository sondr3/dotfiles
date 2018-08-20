;;; core-keybindings.el --- Core keybindings -*- lexical-binding: t -*-

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

;; This is probably the hardest thing by far to configure and properly do in
;; Emacs, at least in my opinion. I could use something like Spacemacs or Doom
;; which has a proper consistent theme for keybindings, but that's no fun.
;; Instead we'll roll our own built around `Evil', `General.el' and `which-key'.
;; Lastly, we'll mimick how I used to do things in Vim (and how Spacemacs and
;; others does things) by letting `SPC' be our leader key and `,' be our major
;; mode leader key. If you are in the `insert' state, you can use `C-SPC' for
;; the leader key and `M-,' for the major mode leader key.

;;; Todos and fixes:

;; TODO Create a hydra to toggle major/minor modes

;;; Code:

;;; Custom bindings

(defvar amalthea-leader-key "SPC"
  "The default leader key for Amalthea.")

(defvar amalthea-leader-secondary-key "C-SPC"
  "The secondary leader key for Amalthea.")

(defvar amalthea-major-leader-key ","
  "The default major mode leader key for Amalthea.")

(defvar amalthea-major-leader-secondary-key "M-,"
  "The secondary major mode leader key for Amalthea.")

;;; Packages

;;; `which-key':
;; This is a really cool package, I initially discovered this from Spacemacs (as
;; I have done with a great many things). What it does is show you any and all
;; keybindings you can complete from the binding you just executed. For example,
;; if you are in Org-mode and run `C-c', `which-key' will show on the bottom of
;; the screen and show all the keybindings you can complete from there. It's
;; really great for discoverability.
(use-package which-key
  :demand t
  :delight
  :commands (which-key-mode)
  :config
  (progn
    (setq which-key-idle-delay 0.3                         ;; Reduce the time before which-key pops up
          which-key-allow-evil-operators t
          which-key-sort-order 'which-key-key-order-alpha) ;; Sort things properly alphabetical
    (which-key-mode)))

;;; `General':
;; `use-package' has a built-in way of binding keys, but after having tried to
;; use it in a slightly more advanced way than just binding keys I've found that
;; it doesn't work as I would've liked it to do. Enter General: it's a whole
;; framework for binding keys in a really nice and consistent manner. We'll also
;; configure our leader keys using the constants we created in the introduction
;; to keybindings.
(use-package general
  :demand t
  :commands (general-define-key general-override-mode general-evil-setup general--simulate-keys)
  :config
  (progn
    (setq general-override-states '(insert emacs hybrid normal visual motion operator replace))
    (general-override-mode)
    (general-evil-setup)
    (general-create-definer amalthea-leader
      :states '(normal insert emacs)
      :prefix amalthea-leader-key
      :non-normal-prefix amalthea-leader-secondary-key)
    (general-create-definer amalthea-major-leader
      :states '(normal insert emacs)
      :prefix amalthea-major-leader-key
      :non-normal-prefix amalthea-major-leader-secondary-key)
    (general-nmap "SPC m" (general-simulate-key "," :which-key "major mode"))))

;;; Default `which-key' prefixes
;; This keeps all the main menus in one place instead of spread throughout the
;; whole project.
(amalthea-leader
  "a" '(:ignore t :wk "applications")
  "b" '(:ignore t :wk "buffers")
  "f" '(:ignore t :wk "files")
  "g" '(:ignore t :wk "git")
  "h" '(:ignore t :wk "help")
  "S" '(:ignore t :wk "spelling")
  "w" '(:ignore t :wk "windows"))

(provide 'core-keybindings)
;;; core-keybindings.el ends here
