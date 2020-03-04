;;; am-packages.el --- Amalthea packaging -*- lexical-binding: t -*-

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
;; There's a ton of different ways of doing package management in Emacs, most of
;; which nowadays revolve around `use-package'. There's also `quelpa',
;; `straight.el', `Borg' and a bunch more. I've tried them all, but there is
;; always some minute thing about them that bothers me. In the end I've ended up
;; configuring the packages I need in a `Nix' expression, allowing me to
;; declarative install dependencies.

;; I still use `use-package' to configure my installed packages, but the
;; `:ensure' functionality of it is disabled. This gives me the best of both
;; worlds, all the great functionality and integration of `use-package' and the
;; declarative packaging that you would have with `straight.el' or `Borg'.

;;; Code:
;; Require `use-package', need I say more?
(eval-and-compile
  (require 'use-package))

(csetq use-package-always-ensure t
       use-package-expand-minimally t      ;; Expand the `use-package' with no bells or whistles
       use-package-always-defer t)         ;; Always defer packages

;;;; `delight':
;; Though you could use `diminish' for making the modeline look better,
;; `delight' is a much better package. Not only can you change the names or hide
;; major-modes from the modeline, you can also nest what minor-mode you want to
;; hide instead of having to do it one at a time.
(use-package delight :demand t)

(provide 'am-packages)

;;; am-packages.el ends here
