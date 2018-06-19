;;; core-ui.el --- Core UI settings -*- lexical-binding: t -*-

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

(defcustom amalthea-mono-font "Fira Mono"
  "The default monospaced font that Amalthea uses."
  :type 'string
  :group 'amalthea)

(defcustom amalthea-serif-font "Fira Sans"
  "The default sans serif font that Amalthea uses."
  :type 'string
  :group 'amalthea)
  
(set-face-attribute 'default nil
		    :family amalthea-mono-font
		    :height 80)
(set-face-attribute 'variable-pitch nil
		    :family amalthea-serif-font
		    :height 80)
(set-frame-font amalthea-mono-font nil t)

(provide 'core-ui)

;;; core-ui.el ends here
