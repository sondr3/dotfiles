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

(defconst amalthea-modules-langs-dir (expand-file-name "langs/" amalthea-modules-dir)
  "Path to configuration of languages in Amalthea.")

(defconst amalthea-modules-tools-dir (expand-file-name "tools/" amalthea-modules-dir)
  "Path to configuration of tools in Amalthea.")

(defconst amalthea-modules-text-dir (expand-file-name "text/" amalthea-modules-dir)
  "Path to configuration of text editing tools in Amalthea.")

(defconst amalthea-modules-org-dir (expand-file-name "org/" amalthea-modules-dir)
  "Path to configuration of org-mode' and related packages in Amalthea.")

;;; Functions
(defun amalthea--byte-compile-module-dirs ()
  "Byte compile all modules that Amalthea bundles."
  (interactive)
  (byte-recompile-directory amalthea-modules-dir 0 t)
  (byte-compile-file buffer-file-name))

;;; Add all the languages to the load path
(eval-and-compile
  (add-to-list 'load-path amalthea-modules-langs-dir t)
  (add-to-list 'load-path amalthea-modules-tools-dir t)
  (add-to-list 'load-path amalthea-modules-text-dir t)
  (add-to-list 'load-path amalthea-modules-org-dir t))

;;; Load modules
(progn ;; Load language modules
  (require '+assembly)
  (require '+elisp)
  (require '+haskell)
  (require '+shell)
  (require '+json)
  (require '+java)
  (require '+nix)
  (require '+typescript))
(progn ;; Load assorted tools
  (require '+docker)
  (require '+nginx))
(progn ;; Load text editing modules
  (require '+latex)
  (require '+markdown))
(progn ;; Load personal configuration
  (require '+org))

(provide 'modules)
;;; modules.el ends here
