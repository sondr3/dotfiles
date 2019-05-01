;;; +lsp.el --- Language Server Protocol -*- lexical-binding: t -*-

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

;; Support for the Language Server Protocol and the various associated packages
;; that use/feed it.

;;; Code:

;;; `lsp-mode':
;; The bread and butter for LSP, the only thing we'll configure is disabling
;; Flymake because we're using Flycheck instead.
(use-package lsp-mode
  :commands lsp
  :delight " Ⓛ"
  :init (csetq lsp-prefer-flymake nil))

;;; `lsp-ui':
;; Gives us some goodies while browsing the code.
(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :ghook ('lsp-mode-hook #'lsp-ui-mode))

;;; `company-lsp':
;; Enables auto-completion for languages that use LSP.
(use-package company-lsp
  :after company
  :commands company-lsp
  :init (push 'company-lsp company-backends))

(provide '+lsp)
;;; +lsp.el ends here
