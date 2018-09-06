;;; org-org.el --- Org mode support -*- lexical-binding: t -*-

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
  :defines (org-export-with-sub-superscripts org-babel-do-load-languages)
  :commands org-babel-do-load-languages
  :config
  (progn
    (delight 'org-indent-mode)
    (setq org-src-fontify-natively t                       ;; Always use syntax highlighting of code blocks
          org-startup-with-inline-images t                 ;; Always show images
          org-startup-indented t                           ;; Indent text according to the current header
          org-hide-emphasis-markers t                      ;; Hides the symbols that makes text bold, italics etc
          org-use-sub-superscripts '{}                     ;; Always use {} to group sub/superscript text
          org-export-with-sub-superscripts '{}             ;; Export with the same syntax as above
          org-preview-latex-default-process 'dvisvgm       ;; Use DVI for LaTeX fragments, not PNG
          org-format-latex-options
          (plist-put org-format-latex-options :scale 1.25) ;; Make the preview a little larger
          org-startup-with-latex-preview t                 ;; Preview LaTeX fragments on startop
          org-pretty-entities t                            ;; Show entities as UTF8-characters when possible
          org-list-allow-alphabetical t)                   ;; Allow lists to be a), etc

    ;; Configure which languages we can use in Org Babel code blocks
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((shell . t)
       (emacs-lisp . t)
       (latex . t)
       (java . t)))

    (defface amalthea--org-math-highlight
      '((t :inherit org-code :slant italic))
      "My own configuration for highlighting math blocks in org-mode"
      :group 'org-faces)

    (add-hook 'org-font-lock-set-keywords-hook
              (lambda ()
                (add-to-list 'org-font-lock-extra-keywords
                             ;; '("\\$\\(.+?\\)\\$"
                             '("\\(\\$\\)\\([^\n\r\t]+?\\)\\(\\$\\)"
                               (1 '(face org-code invisible t))
                               (2 'amalthea--org-math-highlight)
                               (3 '(face org-code invisible t))))))))

(use-package org-indent :delight)

(provide 'org-org)
;;; org-org.el ends here
