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

;; LaTeX gives amazing results but working with it can be really awful, I've
;; honed my configuration for working with it for a few years now and by now it
;; works very well. Though it will probably not work straight out of the box for
;; anyone but me. If you truly want to try it out, you must have the full TeX
;; Live distribution installed, you need to use `latexmk' with `lualatex' and
;; you should copy that configuration from my dotfiles. Once you've done that,
;; lets dive in.

;;; Code:

;;; `auctex':
;; Emacs comes bundled with a very simply TeX-mode, however we want to enhance
;; this by using `auctex' to add a bunch of really nice quality of life changes.
(use-package tex
  :commands (TeX-source-correlate-mode TeX-PDF-mode)
  :functions LaTeX-math-mode
  ;; Enable some extra modes for editing, spelling, auto completion etc
  :ghook ('LaTeX-mode-hook (list #'TeX-fold-mode #'LaTeX-math-mode #'TeX-source-correlate-mode
                                 #'TeX-PDF-mode #'flyspell-mode #'company-mode #'rainbow-delimiters-mode))
  :general
  (amalthea-major-leader 'LaTeX-mode-map
    "TAB" '(align-current :wk "align"))
  :init
  (progn
    (setq-default TeX-master nil)                                 ;; Always ask which file is the master TeX file
    (setq TeX-command-default "latexmk"                           ;; Use `latexmk' to compile documents
          TeX-command-force "latexmk"                             ;; REALLY use `latexmk' to compile documents
          TeX-engine 'lualatex                                    ;; The default engine of choice is `lualatex'
          TeX-auto-save t                                         ;; Save documents automatically when running commands on them
          TeX-parse-self t                                        ;; Don't really know, everyone sets it to `t'
          TeX-save-query nil                                      ;; Don't ask for permission when saving
          TeX-PDF-mode t                                          ;; Compile documents to PDF
          TeX-show-compilation nil                                ;; Don't pop up the compilation buffer, use C-c C-l to show it
          TeX-syntactic-comment t                                 ;; No idea either, no documentation for it
          TeX-clean-confirm t                                     ;; Ask before cleaning temporary files
          TeX-electric-math t                                     ;; Electric opening and closing of math environments
          TeX-electric-sub-and-superscript t                      ;; Same with sub and superscript
          TeX-source-correlate-mode t                             ;; Enable correlation between source and output
          TeX-source-correlate-method 'synctex                    ;; Use `synctex' to sync cursor location to PDF viewer
          TeX-source-correlate-start-server t ; Start the server by default
          LaTeX-babel-hyphen nil                                  ;; Don't aid in hyphenation
          TeX-view-program-selection '((output-pdf "PDF Viewer")) ;; View compiled PDFs in this program
          TeX-view-program-list '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))))

;;; `auctex-latexmk':
;; Adds support for `latexmk' to `auctex', mostly useful for making sure that
;; various minor modes are added to the command line parameters used by
;; `latexmk'.
(use-package auctex-latexmk
  :commands auctex-latexmk-setup
  :init
  (progn
    (setq auctex-latexmk-inherit-TeX-PDF-mode t) ;; Tell `auctex' that we're compiling to a PDF
    (auctex-latexmk-setup)))

;;; `company-auctex':
;; Auto completion for LaTeX buffers. Yes. It's good.
(use-package company-auctex
  :commands company-auctex-init
  :init (company-auctex-init))

;;; `company-math':
;; Adds auto completion for symbols and commands used in LaTeX.
(use-package company-math
  :config
  (progn
    (add-to-list 'company-backends 'company-math-symbols-latex t)
    (add-to-list 'company-backends 'company-math-symbols-unicode t)
    (add-to-list 'company-backends 'company-latex-commands t)))

;;; `magic-latex-buffer':
;; Literally magic. This makes buffers really nice.
(use-package magic-latex-buffer
  :disabled
  :commands magic-latex-buffer
  :ghook ('LaTeX-mode-hook #'magix-latex-buffer)
  :init
  (progn
    (setq magic-latex-enable-block-highlight t ;; Prettify blocks that change their font size
          magic-latex-enable-suscript t        ;; Prettify sub and super script blocks
          magic-latex-enable-pretty-symbols t  ;; Convert latex variables into their UTF8 symbol
          magic-latex-enable-block-align nil   ;; Don't make \centering blocks appear centered in the LaTeX buffer
          magic-latex-enable-inline-image t))) ;; Display images inline in the LaTeX document

;;; `latex-extra':
;; This package adds a bunch of small but nice quality of life enhancements to
;; `auctex', mostly for folding content, moving quickly between sections and
;; better handling of `auto-fill-mode'.
(use-package latex-extra
  :delight
  :defines latex-extra-mode
  :ghook ('LaTeX-mode-hook #'latex-extra-mode))

;;; `reftex':
;; This is a specialized package for labels, references and citations. It is
;; really awesome, and is such a massive help when writing documents that
;; requires references etc.
(use-package reftex
  :delight
  :commands (turn-on-reftex reftex-mode)
  :ghook ('LaTeX-mode-hook #'turn-on-reftex)
  :init
  (progn
    (setq reftex-plug-into-AUCTeX t                                       ;; Enable the integration with `auctex'
          reftex-use-fonts t                                              ;; Prettify things
          reftex-default-bibliography '("~/Code/UiB/bibliography.bib")))) ;; Default location of references

;;; `company-reftex':
;; Enable auto completion for reftex in Company.
(use-package company-reftex
  :config
  (progn
    (add-to-list 'company-backends 'company-reftex-labels t)
    (add-to-list 'company-backends 'company-reftex-citations t)))

;;; `ivy-bibtex':
;; This package is really useful for working with bibliographies, its primary
;; usage is automatic generation of the key for entries and for quickly
;; inserting them as well.
(use-package ivy-bibtex
  :init
  (progn
    (setq ivy-re-builders-alist ;; Required for using this package
          '((ivy-bibtex . ivy--regex-ignore-order)
            (t . ivy--regex-plus))))
  :config
  (progn
    (setq bibtex-dialect 'biblatex     ;; Use a new and "modern" BibTeX format
          bibtex-align-at-equal-sign t ;; Align entries in our `.bib' file at `='

          ;; Configuration for how to format keys for bibliography entries, I've
          ;; changed this to be like how `zotero' does it. I can't remember how
          ;; the default looks, but with how this is configured they keys will
          ;; look like this: `munroe2015PublicKey'. Author name first, then year
          ;; and then name of paper/book etc.
          bibtex-autokey-year-length 4                                       ;; Use full years
          bibtex-autokey-name-year-separator ""                              ;; Don't separate the year and author
          bibtex-autokey-year-title-separator ""                             ;; Or the year and title
          bibtex-autokey-titleword-separator ""                              ;; Or the words in the title
          bibtex-autokey-titlewords 4                                        ;; The key should be four words
          bibtex-autokey-titlewords-stretch 2                                ;; With two extra words from the title
          bibtex-autokey-titleword-length t                                  ;; Use all characters from title
          bibtex-autokey-titleword-case-convert-function 'identity           ;; Preserve casing on title
          ivy-bibtex-default-action 'bibtex-completion-insert-citation       ;; Automatically insert citation
          bibtex-completion-bibliography '("~/Code/UiB/bibliography.bib")))) ;; Default location of bibliography

;;; `ebib':
;; On the other side of the same coin, `ebib' makes managing and editing your
;; bibliography amazingly easy. It builds on top of the previous configuration,
;; but gives you a full mode wherein you can change, update and fix bibliography
;; entries. It's amazing.
(use-package ebib
  :config
  (progn
    (setq ebib-bibtex-dialect 'biblatex                              ;; Use same dialect as BibTeX
          ebib-preload-bib-files '("~/Code/UiB/bibliography.bib")))) ;; Default location of bibliography

(provide 'text-latex)
;;; text-latex.el ends here
