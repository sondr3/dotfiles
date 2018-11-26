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
(with-eval-after-load 'ox-latex
  (setq org-latex-classes
        '(("memoir-book"
           "\\documentclass[12pt,a4paper,oneside]{memoir}
            [DEFAULT-PACKAGES]

            \\defaultfontfeatures{Ligatures=TeX}
            \\newfontfeature{Microtype}{protrusion=default;expansion=default}
            \\setmainfont{Linux Libertine O}
            \\setsansfont{Linux Biolinum O}
            \\setmonofont{DejaVu Sans Mono}[Scale=MatchLowercase]

            [PACKAGES]

            \\addbibresource{/Users/sondre/Code/UiB/bibliography.bib}
            \\chapterstyle{veelo}
            \\headstyles{memman}
            \\pagestyle{ruled}

            [EXTRA]"
           ("\\chapter{%s}" . "\\chapter*{%s}")
           ("\\section{%s}" . "\\section*{%s}")
           ("\\subsection{%s}" . "\\subsection*{%s}")
           ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
           ("\\paragraph{%s}" . "\\paragraph*{%s}")
           ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
          ("memoir"
           "\\documentclass[12pt,a4paper,oneside]{memoir}
            [DEFAULT-PACKAGES]

            \\defaultfontfeatures{Ligatures=TeX}
            \\newfontfeature{Microtype}{protrusion=default;expansion=default}
            \\setmainfont{Linux Libertine O}
            \\setsansfont{Linux Biolinum O}
            \\setmonofont{DejaVu Sans Mono}[Scale=MatchLowercase]

            [PACKAGES]

            \\addbibresource{/Users/sondre/Code/UiB/bibliography.bib}
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
           ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))
  (setq org-format-latex-header
        "\\documentclass[12pt,a4paper,oneside]{memoir}
           [DEFAULT-PACKAGES]
           [NO-PACKAGES]
           \\defaultfontfeatures{Ligatures=TeX}
           \\newfontfeature{Microtype}{protrusion=default;expansion=default}
           \\setmainfont{Linux Libertine O}
           \\setsansfont{Linux Biolinum O}
           \\setmonofont{DejaVu Sans Mono}[Scale=MatchLowercase]
           \\pagestyle{empty}             % do not remove
           % The settings below are copied from fullpage.sty
           \\setlength{\\textwidth}{\\paperwidth}
           \\addtolength{\\textwidth}{-3cm}
           \\setlength{\\oddsidemargin}{1.5cm}
           \\addtolength{\\oddsidemargin}{-2.54cm}
           \\setlength{\\evensidemargin}{\\oddsidemargin}
           \\setlength{\\textheight}{\\paperheight}
           \\addtolength{\\textheight}{-\\headheight}
           \\addtolength{\\textheight}{-\\headsep}
           \\addtolength{\\textheight}{-\\footskip}
           \\addtolength{\\textheight}{-3cm}
           \\setlength{\\topmargin}{1.5cm}
           \\addtolength{\\topmargin}{-2.54cm}")
  (setq org-latex-compiler "lualatex"                       ;; Use a modern LaTeX compiler
        org-latex-default-class "memoir"                    ;; Use my own class by default
        org-latex-default-table-environment "tabularx"      ;; Use a better table formatter
        org-latex-tables-booktabs t                         ;; Always use booktabs for better looking tables
        org-latex-prefer-user-labels t                      ;; Prefer labels I make myself please
        org-latex-listings t                                ;; Make SRC blocks export to code blocks in LaTeX
        org-latex-pdf-process
        (list "latexmk -pvc- %f -cd %o")                    ;; Use `latexmk' to generate PDF
        org-latex-listings-options                          ;; Configure source code exporting
        '(("frame" "tb")                                    ;; Single lines at the top and bottom of frame
          ("columns" "fullflexible")                        ;; Fix spacing in source code
          ("flexiblecolumns" "true")                        ;; Same as above
          ("numbers" "left")                                ;; Show line numbers on the left
          ("numberstyle" "\\ttfamily\\color{gray}\\tiny")   ;; Monospaced gray tiny line numbers
          ("showstringspaces" "false")                      ;; Don't show spaces in strings as underlines
          ("basicstyle" "\\ttfamily\\footnotesize"))        ;; Use footnote sized monospace font
        org-latex-default-packages-alist                    ;; Configure default packages inserted into LaTeX classes
        '(("AUTO" "polyglossia" t)                          ;; Polyglossia for language settings, automatically configured
          ("" "fontspec" t)                                 ;; Fancy fonts for OpenType fonts in LuaLaTeX
          ("" "microtype" t)                                ;; Micro-typography, for when you need even more typography
          ("" "geometry" t)                                 ;; Enable configuring the geometry of the pages
          ("" "subfiles" t)                                 ;; Enables splitting up large .tex files into smaller parts
          ("" "float" t)                                    ;; Float environments in LaTeX
          ("font=small,labelfont=bf,format=hang" "caption") ;; Make the font in captions smaller
          ("" "amsfonts" t)                                 ;; Math fonts
          ("" "amssymb" t)                                  ;; Math symbols
          ("" "mathtools" t)                                ;; Extra math tools
          ("shortlabels" "enumitem" t)                      ;; Enumerate environment with an option to change numbering quickly
          ("" "multirow" t)                                 ;; Lines that span multiple columns etc in tables
          ("" "tabularx" t)                                 ;; A better table environment
          ("" "hyperref" t)                                 ;; Links inside the generated PDFs
          ("" "tikz" t)                                     ;; Awesome technical diagrams and everything in between
          ("edges" "forest" t)                              ;; Quick and really easy way to draw graphs
          ("" "graphicx" t)                                 ;; Embed graphics in LaTeX documents
          ("" "xcolor" t)                                   ;; Color utility for text etc
          ("" "colortbl" t)                                 ;; Color rows and columns in tables
          ("" "array" t)                                    ;; Arrays, like tables, but not
          ("" "listings" t))                                ;; Display source code in LaTeX-documents
        org-latex-packages-alist                            ;; Extra packages that we load after the default ones
        '(("autostyle,strict,autopunct" "csquotes" t)       ;; Quoting and citing made easy
          ("style=ieee,backend=biber" "biblatex" t))        ;; Bibliography and citing
        org-latex-hyperref-template "\\hypersetup{\n colorlinks=true,\n pdfauthor={%a},\n pdftitle={%t},\n pdfkeywords={%k},\n pdfsubject={%d},\n pdfcreator={%c},\n pdflang={%L}}\n")

  ;; Add Java to the list of languages for listings
  (add-to-list 'org-latex-listings-langs '(java "Java") t))

;;; `by-backend':
;; This is a really neat macro that allows us to use different export settings
;; for different exports, i.e. export some LaTeX-code as an image when
;; exporting to HTML but stay as LaTeX when exporting to LaTeX.
(defmacro by-backend (&rest body)
  `(case org-export-current-backend ,@body))

(provide 'org-latex-export)
;;; org-latex-export.el ends here
