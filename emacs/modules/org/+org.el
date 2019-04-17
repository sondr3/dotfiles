;;; +org.el --- Org mode support -*- lexical-binding: t -*-

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

;; Use org-mode, it's awesome.

;;; Code:

;;; `org':
;; Org-mode is an amazing piece of work, it can more or less do everything that
;; you can think of, spread sheets, interactive coding, notes, exporting to
;; everything under the sun and so on
(use-package org
  :defines (org-export-with-sub-superscripts org-babel-do-load-languages)
  :commands org-babel-do-load-languages
  :config
  (progn
    (csetq org-src-fontify-natively t                       ;; Always use syntax highlighting of code blocks
           org-startup-with-inline-images t                 ;; Always show images
           org-startup-indented t                           ;; Indent text according to the current header
           org-hide-emphasis-markers t                      ;; Hides the symbols that makes text bold, italics etc
           org-use-sub-superscripts '{}                     ;; Always use {} to group sub/superscript text
           org-export-with-sub-superscripts '{}             ;; Export with the same syntax as above
           org-preview-latex-default-process 'dvisvgm       ;; Use dvisvgm for better quality LaTeX fragments
           org-format-latex-options
           (plist-put org-format-latex-options :scale 1.25) ;; Make the preview a little larger
           org-catch-invisible-edits 'smart                 ;; Smart editing of hidden regions
           org-highlight-latex-and-related '(latex)         ;; Highlight LaTeX fragments, snippets etc
           org-pretty-entities t                            ;; Show entities as UTF8-characters when possible
           org-list-allow-alphabetical t                    ;; Allow lists to be a) etc
           org-confirm-babel-evaluate nil                   ;; Don't bug about executing code all the time
           org-babel-python-command "python3")              ;; Newer is always better

    ;; Configure which languages we can use in Org Babel code blocks
    ;; NOTE: This slows down the startup of Org-mode a little bit
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((shell . t)
       (emacs-lisp . t)
       (dot . t)
       (latex . t)
       (python .t)
       (java . t)))))

;; I don't want the mode line to show that org-indent-mode is active
(use-package org-indent :after org :delight)

(use-package org-ref
  :init
  (progn
    (csetq reftex-default-bibliography '("~/Code/UiB/bibliography.bib")
           org-ref-completion-library 'org-ref-ivy-bibtex)))

;; Load the rest of the org-mode configuration
(require '+agenda)
(require '+bindings)
(require '+capture)
(require '+beamer)
(require '+latex)

(provide '+org)
;;; +org.el ends here
