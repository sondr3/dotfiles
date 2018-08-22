;;; base-help.el --- Help support for Amalthea -*- lexical-binding: t -*-

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

;; Configuration for making the help buffers more helpful.

;;; Code:

;;; `helpful':
;; The name basically tells you what it does, it makes the help buffer show a
;; lot more information. We bind the `helpful' functions that have no
;; counterpart in Emacs and remap those that do.
(use-package helpful
  :general
  (amalthea-leader
    "h c" '(helpful-command :wk "describe command")
    "h d" '(helpful-at-point :wk "describe at point")
    "h f" '(describe-function :wk "describe function")
    "h F" '(helpful-function :wk "helpful function")
    "h k" '(describe-key :wk "describe key")
    "h m" '(helpful-macro :wk "describe macro")
    "h M" '(describe-mode :wk "describe mode")
    "h v" '(describe-variable :wk "describe variable"))
  (:keymaps 'override
            [remap describe-function] 'helpful-callable
            [remap describe-key] 'helpful-key
            [remap describe-variable] 'helpful-variable))

(provide 'base-help)
;;; base-help.el ends here
