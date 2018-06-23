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

;; TODO: Currently only configured for use on Linux.

;; Contains settings related to making Emacs work better on various operating
;; systems. Currently only configures and enables copying and pasting between
;; Emacs and X11 and choses the builtin tooltips over GTK.

;;; Code:

(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)
      select-enable-clipboard t
      select-enable-primary t
      x-gtk-use-system-tooltips nil
      x-underline-at-descent-line t)

(provide 'core-os)
;;; core-os.el ends here
