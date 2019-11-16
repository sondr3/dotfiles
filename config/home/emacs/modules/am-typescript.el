;;; am-typescript.el --- Amalthea TypeScript configuration -*- lexical-binding: t -*-

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
;; TypeScript support.

;;; Code:
;;; `prettier-js':
;; Automatically prettify buffers.
(use-package prettier-js)

;;; `typescript-mode':
;; Quick configuration for TypeScript, setting the indentation and enabling
;; `prettier-js' and `lsp' for it.
(use-package typescript-mode
  :ghook ('typescript-mode-hook (list #'lsp #'prettier-js-mode))
  :init (csetq typescript-indent-level amalthea-tab-width
               js-indent-level amalthea-tab-width))

(provide 'am-typescript)

;;; am-typescript.el ends here
