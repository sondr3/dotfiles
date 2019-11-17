;;; init.el --- Amalthea configuration -*- lexical-binding: t -*-

;; Author: Sondre Nilsen
;; Maintainer: Sondre Nilsen
;; Homepage: https://github.com/sondr3/amalthea

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

;; Isn't the view from here beautiful?

;;; Code:

;;; Initialization
;; Compatibility with versions 26 and below.
(unless (boundp 'early-init-file)
  (load (expand-file-name "early-init" user-emacs-directory)))

;; Check for Emacs version
(eval-when-compile
  (unless (not (version< emacs-version "26"))
    (error "You are using Emacs %s, Amalthea requires version 26 or higher" emacs-version)))

;; Load Amalthea
(require 'am-core (expand-file-name "core/am-core" user-emacs-directory))

;; And once all is said and done, start emacsclient in the background so that we
;; can connect to it from other windows or the terminal.
(require 'server)
(unless (server-running-p)
  (server-start))

;; (provide 'init)
;;; init.el ends here
