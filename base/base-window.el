;;; base-window.el --- Window management -*- lexical-binding: t -*-

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

;; Windows 10

;;; Code:

(defhydra hydra-zoom (:color red :hint nil)
  "zoom"
  ("k" text-scale-increase "in")
  ("j" text-scale-decrease "out")
  ("r" (text-scale-adjust 0) "reset" :color blue)
  ("q" nil "quit" :color blue))

(amalthea-leader 'normal
  "w f" '(toggle-frame-fullscreen :wk "fill screen")
  "w m" '(toggle-frame-maximized :wk "maximize")
  "w z" '(hydra-zoom/body :wk "zoom"))

(provide 'base-window)

;;; base-window.el ends here
