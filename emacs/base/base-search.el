;;; base-search.el --- Search support -*- lexical-binding: t -*-

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

;; Gives you super powered searching via `ripgrep' and `deadgrep'.

;;; Code:

;;; `deadgrep':
;; Sweet, sweet searching.
(use-package deadgrep
  :general
  (amalthea-leader
    "a s" '(deadgrep :wk "ripgrep")))

(provide 'base-search)
;;; base-search.el ends here
