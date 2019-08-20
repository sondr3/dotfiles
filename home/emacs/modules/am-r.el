;;; am-r.el --- Amalthea R configuration -*- lexical-binding: t -*-

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

;; commentary

;;; Code:
;;; `ess':
;; Emacs Speaks Statistics
(use-package ess
  :config
  (progn
    (csetq ess-use-flymake nil
           ;; Follow Hadley Wickham's R style guide
           ess-first-continued-statement-offset 2
           ess-continued-statement-offset 0
           ess-expression-offset 2
           ess-nuke-trailing-whitespace-p t
           ess-default-style 'DEFAULT)))

(provide 'am-r)

;;; am-r.el ends here
