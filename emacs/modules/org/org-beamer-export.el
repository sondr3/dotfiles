;;; org-beamer-export.el --- Org-mode Beamer export configuration -*- lexical-binding: t -*-

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

;; Configuration for creating presentations using LaTeX and Beamer.

;;; Code:

;; We need our own class for Beamer because the builtin one is ugly and
;; doesn't support LuaLaTeX, we just need to make sure not to include any of
;; the default packages.
(use-package ox-latex
  :config
  (add-to-list 'org-latex-classes
               '("beamer"
                 "\\documentclass[presentation]{beamer}
                \\usepackage[AUTO]{polyglossia}
                \\usepackage{fontspec}
                \\usepackage{microtype}
                \\usepackage{tabularx}
                \\usepackage{booktabs}
                \\usepackage{listings}
                \\usepackage{pgfpages}
                \\setbeameroption{show notes on second screen=right}
                [NO-DEFAULT-PACKAGES]
                [NO-PACKAGES]
                [EXTRA]"
		             ("\\section{%s}" . "\\section*{%s}")
		             ("\\subsection{%s}" . "\\subsection*{%s}")
		             ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
               t))

(use-package ox-beamer
  :config
  (csetq org-beamer-theme "metropolis" ;; Use the `metropolis' theme
         org-beamer-frame-level 2))    ;; Give the slides some more depth

(provide 'org-beamer-export)
;;; org-beamer-export.el ends here
