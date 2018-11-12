;;; modules.el --- Importing modules -*- lexical-binding: t -*-

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

;;; Constants

(defconst amalthea-modules-langs-dir (concat amalthea-modules-dir "langs/")
  "Path to configuration of languages in Amalthea.")

(defconst amalthea-modules-tools-dir (concat amalthea-modules-dir "tools/")
  "Path to configuration of tools in Amalthea.")

(defconst amalthea-modules-text-dir (concat amalthea-modules-dir "text/")
  "Path to configuration of text editing tools in Amalthea.")

(defconst amalthea-modules-org-dir (concat amalthea-modules-dir "org/")
  "Path to configuration of org-mode and related packages in Amalthea.")

;;; Functions
(defun amalthea--byte-compile-module-dirs ()
  (interactive)
  (byte-recompile-directory amalthea-modules-dir 0 t)
  (byte-compile-file buffer-file-name))

;;; Add all the languages to the load path
(eval-and-compile
  (add-to-list 'load-path amalthea-modules-langs-dir)
  (add-to-list 'load-path amalthea-modules-tools-dir)
  (add-to-list 'load-path amalthea-modules-text-dir)
  (add-to-list 'load-path amalthea-modules-org-dir))

;;; Languages
(require 'lang-elisp)
(require 'lang-haskell)
(require 'lang-markdown)
(require 'lang-shell)
(require 'lang-json)
(require 'lang-web)
(require 'lang-rust)

;;; Tools
(require 'tool-configuration)
(require 'tool-docker)
(require 'tool-nginx)

;;; Org
(require 'org-org)

;;; Text editing
(require 'text-latex)

(provide 'modules)
;;; modules.el ends here
