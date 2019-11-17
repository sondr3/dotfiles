;;; am-assembly.el --- Amalthea Assembly configuratino -*- lexical-binding: t -*-

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

;;; Code:
;; Adds support for `nasm-mode' to supercede the builtin `asm-mode'.

(add-to-list 'auto-mode-alist '("\\.asm\\'" . nasm-mode))


(provide 'am-assembly)

;;; am-assembly.el ends here
