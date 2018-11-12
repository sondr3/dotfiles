;;; text-org.el --- Org mode support -*- lexical-binding: t -*-

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
          org-latex-listings t                             ;; Make SRC blocks export to code blocks in LaTeX
          org-latex-pdf-process (list "latexmk -pvc- %f")  ;; Use `latexmk' to generate PDF
          org-pretty-entities t                            ;; Show entities as UTF8-characters when possible
          org-list-allow-alphabetical t)                   ;; Allow lists to be a), etc

    ;; Configure which languages we can use in Org Babel code blocks
    ;; NOTE: This slows down the startup of Org-mode a little bit
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((shell . t)
       (emacs-lisp . t)
       (java . t)))

    (add-to-list 'org-latex-classes
                 '("memoir"
                   "\\documentclass[12pt,a4paper,oneside,article]{memoir}
                    [NO-DEFAULT-PACKAGES]
                    \\usepackage{polyglossia}
                    \\setdefaultlanguage{english}
                    \\usepackage{fontspec}

                    \\defaultfontfeatures{Ligatures=TeX}
                    \\newfontfeature{Microtype}{protrusion=default;expansion=default}
                    \\usepackage[final]{microtype}
                    \\setmainfont{Linux Libertine O}
                    \\setsansfont{Linux Biolinum O}
                    \\setmonofont{DejaVu Sans Mono}

                    \\usepackage{subfiles}
                    \\usepackage{tabulary}
                    \\usepackage{tabu}
                    \\usepackage{booktabs}
                    \\usepackage{multirow}
                    \\usepackage{float}
                    \\usepackage{amsmath,amsfonts,amssymb}
                    \\usepackage{mathtools}
                    \\usepackage[shortlabels]{enumitem}
                    \\usepackage{xcolor}
                    \\usepackage{graphicx}
                    \\usepackage{hyperref}
                    \\usepackage{xcolor, colortbl, array}
                    \\usepackage{listings}
                    \\usepackage{color}
                    \\usepackage{chngcntr}

                    [PACKAGES]

                    \\hypersetup{colorlinks = true}
                    \\lstset{language=Java,frame=tb,
                      breaklines=true,
                      breakatwhitespace=true,
                      keepspaces=true,
                      columns=fullflexible,
                      showspaces=false,
                      showstringspaces=false,
                      showtabs=false,
                      basicstyle=\\ttfamily\\footnotesize
                    }

                    \\usepackage{chngcntr}
                    \\counterwithin{table}{section}
                    \\numberwithin{equation}{chapter}
                    \\counterwithin{figure}{section}
                    \\setenumerate[0]{label= (\\alph*)}
                    \\AtBeginDocument{\\counterwithin{lstlisting}{section}}
                    \\counterwithout{section}{chapter}

                    \\chapterstyle{hangnum}
                    \\pagestyle{ruled}
                    [EXTRA]"
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")
                   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

    ;; For some reason math in between $$...$$ isn't highlighted in any way, and
    ;; that annoys me. After some major regexp-fu I was able to hack this
    ;; together, it now uses the same kind of highlighting as code
    (defface amalthea--org-math-highlight
      '((t :inherit org-code :slant italic))
      "My own configuration for highlighting math blocks in org-mode"
      :group 'org-faces)

    (add-hook 'org-font-lock-set-keywords-hook
              (lambda ()
                (add-to-list 'org-font-lock-extra-keywords
                             ;; '("\\$\\$\\(.+?\\)\\$\\$"
                             '("\\(\\$\\$\\)\\([^\n\r\t]+?\\)\\(\\$\\$\\)"
                               (1 '(face org-code invisible t))
                               (2 'amalthea--org-math-highlight)
                               (3 '(face org-code invisible t))))))))

;; I don't want the mode line to show that org-indent-mode is active
(use-package org-indent :after org :delight)

(provide 'text-org)
;;; text-org.el ends here
