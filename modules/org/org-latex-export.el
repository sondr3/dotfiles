;;; org-latex-export.el --- Org-mode LaTeX export support -*- lexical-binding: t -*-

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

;; Configuration for exporting to LaTeX in org-mode.

;;; Code:

;;; `org-latex'
;; Org-mode has some really amazing exporting options, LaTeX included, but I
;; find the default configuration fairly lacking, so we'll add a bunch of
;; changes and add a custom LaTeX class.
(use-package ox-latex
  :config
  (progn
    (unless (boundp 'org-latex-classes)
      (setq org-latex-classes nil))
    ;; Add out default LaTeX class before referencing it
    (add-to-list 'org-latex-classes
                 '("memoir-book"
                   "\\documentclass[12pt,a4paper,oneside]{memoir}
                    [DEFAULT-PACKAGES]

                    \\setdefaultlanguage{english}
                    \\defaultfontfeatures{Ligatures=TeX}
                    \\newfontfeature{Microtype}{protrusion=default;expansion=default}
                    \\setmainfont{Linux Libertine O}
                    \\setsansfont{Linux Biolinum O}
                    \\setmonofont{DejaVu Sans Mono}[Scale=MatchLowercase]

                    [PACKAGES]

                    \\chapterstyle{veelo}
                    \\headstyles{memman}
                    \\pagestyle{ruled}

                    [EXTRA]"
                   ("\\chapter{%s}" . "\\chapter*{%s}")
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")
                   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
    (add-to-list 'org-latex-classes
                 '("memoir"
                   "\\documentclass[12pt,a4paper,oneside,article]{memoir}
                    [DEFAULT-PACKAGES]

                    \\setdefaultlanguage{english}
                    \\defaultfontfeatures{Ligatures=TeX}
                    \\newfontfeature{Microtype}{protrusion=default;expansion=default}
                    \\setmainfont{Linux Libertine O}
                    \\setsansfont{Linux Biolinum O}
                    \\setmonofont{DejaVu Sans Mono}[Scale=MatchLowercase]

                    [PACKAGES]

                    \\counterwithin{table}{section}
                    \\numberwithin{equation}{chapter}
                    \\counterwithin{figure}{section}
                    \\setenumerate[0]{label= (\\alph*)}
                    \\AtBeginDocument{\\counterwithin{lstlisting}{section}}
                    \\counterwithout{section}{chapter}
                    \\settocdepth{subsection}
                    \\setsecnumdepth{subsection}
                    \\headstyles{memman}
                    \\pagestyle{ruled}

                    [EXTRA]"
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")
                   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
    (setq org-latex-compiler "lualatex"                      ;; Use a modern LaTeX compiler
          org-latex-default-class "memoir"                   ;; Use my own class by default
          org-latex-default-table-environment "tabularx"     ;; Use a better table formatter
          org-latex-tables-booktabs t                        ;; Always use booktabs for better looking tables
          org-latex-caption-above
          '(table image src-block special-block)             ;; Put captions below everything
          org-latex-prefer-user-labels t                     ;; Prefer labels I make myself please
          org-latex-listings t                               ;; Make SRC blocks export to code blocks in LaTeX
          org-latex-pdf-process
          (list "latexmk -pvc- %f -cd %o")            ;; Use `latexmk' to generate PDF
          org-latex-listings-options                         ;; Configure source code exporting
          '(("frame" "tb")
            ("breaklines" "true")
            ("breakatwhitespace" "true")
            ("keepspaces" "true")
            ("columns" "fullflexible")
            ("showspaces" "false")
            ("showstringspaces" "false")
            ("showtabs" "false")
            ("basicstyle" "\\ttfamily\\footnotesize"))
          org-latex-default-packages-alist
          '(("" "polyglossia" t)
            ("" "fontspec" t)
            ("final" "microtype" t)
            ("" "subfiles" t)
            ("" "multirow" t)
            ("" "float" t)
            ("" "amsmath" t)
            ("" "amsfonts" t)
            ("" "amssymb" t)
            ("shortlabels" "enumitem" t)
            ("" "hyperref" t)
            ("" "tikz" t)
            ("edges" "forest" t)
            ("" "graphicx" t)
            ("" "color" t)
            ("" "xcolor" t)
            ("" "colortbl" t)
            ("" "array" t)
            ("" "listings" t))
          org-latex-hyperref-template "\\hypersetup{\n colorlinks=true,\n pdfauthor={%a},\n pdftitle={%t},\n pdfkeywords={%k},\n pdfsubject={%d},\n pdfcreator={%c},\n pdflang={%L}}\n")))

(provide 'org-latex-export)
;;; org-latex-export.el ends here
