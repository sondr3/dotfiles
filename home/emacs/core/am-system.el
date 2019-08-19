;;; am-system.el --- Amalthea system configuration -*- lexical-binding: t -*-

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

;; Neptune (NixOS): Configures and enables copying and pasting between Emacs and
;; X11 and choses the builtin tooltips over GTK.

;;; Code:
(defvar x-gtk-use-system-tooltips nil)
(csetq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING) ;; Magic voodoo
       select-enable-clipboard t                                      ;; Cut and paste from the actual clipboard
       select-enable-primary t                                        ;; Use the primary clipboard
       x-gtk-use-system-tooltips nil                                  ;; Use the builtin Emacs tooltips
       x-underline-at-descent-line t                                  ;; Fix for not using GTK tooltips
       amalthea-font-size 120                                         ;; Make the font smaller on Neptune
       amalthea-line-spacing 0.10)                                    ;; And the spacing a little less

(provide 'am-system)

;;; am-system.el ends here
