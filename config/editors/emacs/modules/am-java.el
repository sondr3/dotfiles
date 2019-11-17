;;; am-java.el --- Amalthea Java configuration -*- lexical-binding: t -*-

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
;; Because fuck me, right? No, I only use it so that indentation and
;; auto-completion works when taking notes, I would never, ever write Java in
;; Emacs. I don't hate myself /that/ much.

;;; Code:

(general-add-hook 'java-mode-hook #'electric-pair-local-mode)

(provide 'am-java)

;;; am-java.el ends here
