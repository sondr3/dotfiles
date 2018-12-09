;;; lang-json.el --- JSON support for Amalthea -*- lexical-binding: t -*-

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

;; Simple configuration for editing and viewing JSON files.

;;; Code:

;;; `json-mode':
;; This is really nothing fancy, we bind some keys and set the indentation to
;; it's proper size.
(use-package json-mode
  :general
  (amalthea-major-leader 'json-mode-map
    "r" '(json-mode-beautify :wk "reformat buffer")
    "p" '(json-mode-show-path :wk "show JSON keys")
    "P" '(json-mode-kill-path :wk "yank JSON keys"))
  :init (csetq js-indent-level 2))

(provide 'lang-json)
;;; lang-json.el ends here
