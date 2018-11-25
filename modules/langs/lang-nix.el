;;; lang-nix.el --- Nix support -*- lexical-binding: t -*-

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

;; Nix language support for Emacs, for more information, see
;; `https://nixos.org/'. This adds support for indenting and auto completion for
;; both `.nix' configuration files and NixOS services, programs etc.

;;; Code:

;;; `nix-mode':
;; Adds support for editing and working with Nix expressions, we don't even need
;; configuration for this! Set it and forget it.
(use-package nix-mode
  :mode "\\.nix\\'")

(provide 'lang-nix)
;;; lang-nix.el ends here
