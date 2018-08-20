;;; org-mode.el --- Org mode support -*- lexical-binding: t -*-

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

;; Minor configuration for org-mode.

;;; Code:

;;; `org':
;; Some very minor configuration for org-mode.
(use-package org
  :delight org-indent-mode
  :defines (org-export-with-sub-superscripts org-babel-do-load-languages)
  :config
  (progn
    (setq org-src-fontify-natively t           ;; Always use syntax highlighting of code blocks
          org-startup-with-inline-images t     ;; Always show images
          org-startup-indented t               ;; Indent text according to the current header
          org-hide-emphasis-markers t          ;; Hides the symbols that makes text bold, italics etc
          org-use-sub-superscripts '{}         ;; Always use {} to group sub/superscript text
          org-export-with-sub-superscripts '{} ;; Export with the same syntax as above
          org-pretty-entities t                ;; Show entities as UTF8-characters when possible
          org-list-allow-alphabetical t)))     ;; Allow lists to be a), etc

(provide 'org-mode)
;;; org.el ends here
