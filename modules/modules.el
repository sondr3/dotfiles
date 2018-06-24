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

;;; Functions
(defun amalthea--byte-compile-module-dirs ()
  "TODO")

;;; Add all the languages to the load path
(eval-and-compile
  (add-to-list 'load-path amalthea-modules-langs-dir))

(require 'elisp)

(provide 'modules)
;;; modules.el ends here
