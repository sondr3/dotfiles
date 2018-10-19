;;; text-latex.el --- LaTeX support -*- lexical-binding: t -*-

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

(use-package tex
  :commands (TeX-source-correlate-mode TeX-PDF-mode)
  :functions LaTeX-math-mode
  :ghook ('LaTeX-mode-hook (list #'TeX-fold-mode #'LaTeX-math-mode #'TeX-source-correlate-mode
                                 #'TeX-PDF-mode #'flyspell-mode #'company-mode #'rainbow-delimiters-mode))
  :general
  (amalthea-major-leader 'LaTeX-mode-map
    "TAB" '(align-current :wk "align"))
  :init
  (progn
    (setq-default TeX-master nil)
    (setq TeX-command-default "latexmk"
          TeX-command-force "latexmk"
          TeX-engine 'lualatex
          TeX-auto-save t
          TeX-parse-self t
          TeX-save-query nil
          TeX-PDF-mode t
          TeX-show-compilation nil
          TeX-syntactic-comment t
          TeX-clean-confirm t
          TeX-electric-math t
          TeX-electric-sub-and-superscript t
          TeX-source-correlate-mode t
          TeX-source-correlate-method 'synctex
          TeX-source-correlate-start-server t
          LaTeX-babel-hyphen nil
          LaTeX-fill-break-at-separators nil
          TeX-view-program-selection '((output-pdf "PDF Viewer"))
          TeX-view-program-list '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))))

(use-package auctex-latexmk
  :commands auctex-latexmk-setup
  :init
  (progn
    (setq auctex-latexmk-inherit-TeX-PDF-mode t)
    (auctex-latexmk-setup)))

(use-package company-auctex
  :commands company-auctex-init
  :init (company-auctex-init))

(use-package company-math
  :config
  (progn
    (add-to-list 'company-backends 'company-math-symbols-latex)
    (add-to-list 'company-backends 'company-math-symbols-unicode)
    (add-to-list 'company-backends 'company-latex-commands)))

(use-package magic-latex-buffer
  :disabled
  :commands magic-latex-buffer
  :ghook ('LaTeX-mode-hook #'magix-latex-buffer)
  :init
  (progn
    (setq magic-latex-enable-block-highlight t
          magic-latex-enable-suscript t
          magic-latex-enable-pretty-symbols t
          magic-latex-enable-block-align nil
          magic-latex-enable-inline-image t)))

(use-package latex-extra
  :delight
  :commands latex-extra-mode
  :ghook ('LaTeX-mode-hook #'latex-extra-mode))

(use-package reftex
  :delight
  :commands (turn-on-reftex reftex-mode)
  :ghook ('LaTeX-mode-hook #'turn-on-reftex)
  :init
  (progn
    (setq reftex-plug-into-AUCTeX t
          reftex-use-fonts t
          reftex-cite-prompt-optional-args t
          reftex-default-bibliography '("~/Code/UiB/bibliography.bib")
          reftex-toc-split-windows-fraction 0.2)))

(use-package company-reftex
  :config
  (progn
    (add-to-list 'company-backends 'company-reftex-labels)
    (add-to-list 'company-backends 'company-reftex-citations)))

(use-package ivy-bibtex
  :init
  (progn
    (setq ivy-re-builders-alist
          '((ivy-bibtex . ivy--regex-ignore-order)
            (t . ivy--regex-plus))))
  :config
  (progn
    (setq bibtex-dialect 'biblatex
          bibtex-align-at-equal-sign t
          bibtex-text-indentation 20
          bibtex-autokey-year-length 4
          bibtex-autokey-name-year-separator ""
          bibtex-autokey-year-title-separator ""
          bibtex-autokey-titleword-separator ""
          bibtex-autokey-titlewords 4
          bibtex-autokey-titlewords-stretch 2
          bibtex-autokey-titleword-length t
          bibtex-autokey-titleword-case-convert-function 'identity
          ivy-bibtex-default-action 'bibtex-completion-insert-citation
          bibtex-completion-bibliography '("~/Code/UiB/bibliography.bib"))))

(use-package ebib
  :config
  (progn
    (setq ebib-bibtex-dialect 'biblatex
          ebib-preload-bib-files '("~/Code/UiB/bibliography.bib"))))

(provide 'text-latex)
;;; text-latex.el ends here
