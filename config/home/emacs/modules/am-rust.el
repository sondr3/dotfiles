;;; am-rust.el --- Rust support for Amalthea -*- lexical-binding: t -*-

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

;;

;;; Code:

(use-package toml-mode)

(use-package rustic
  :mode ("\\.rs$" . rustic-mode)
  :ghook #'lsp
  :config
  (progn
    (csetq rustic-lsp-server 'rust-analyzer
           rustic-format-trigger 'on-save)))

(use-package cargo
  :after rust-mode
  :ghook #'rustic-mode-hook)

(provide 'am-rust)

;;; am-rust.el ends here
