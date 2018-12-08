;;; core-packages.el --- Package configuration -*- lexical-binding: t -*-

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
;; declarative install dependencies and configure them without even using
;; `use-package'.

;;; Code:
(require 'cl-lib)

(defmacro csetq (&rest body)
  "A simple and better version of `setq' that also respects if a
  variable has a `custom-set' property. Works just like the good
  old version, but better, because you can also add comments to
  assignments."
  `(progn
     ,@(cl-loop for (var val _) on body by 'cdddr
                collect `(funcall (or (get ',var 'custom-set) #'set)
                                  ',var ,val))))

;;; `delight':
;; Though you could use `diminish' for making the modeline look better,
;; `delight' is a much better package. Not only can you change the names or hide
;; major-modes from the modeline, you can also nest what minor-mode you want to
;; hide instead of having to do it one at a time.
(require 'delight)

(provide 'core-packages)
;;; core-packages.el ends here
