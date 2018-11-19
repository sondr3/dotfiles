;;; init.el --- initialization for amalthea -*- lexical-binding: t -*-

;; Author: Sondre Nilsen
;; Maintainer: Sondre Nilsen
;; Version: 0.1
;; Package-Requires: ((emacs "27"))
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

;; Compatibility with versions 26 and below.
(unless (boundp 'early-init-file)
  (load (concat (file-name-directory load-file-name) "early-init")))

;; Load Amalthea
(require 'core (concat user-emacs-directory "core/core"))

;; And once all is said and done, start emacsclient in the background so that we
;; can connect to it from other windows or the terminal.
(require 'server)
(unless (server-running-p)
  (server-start))

;; (provide 'init)
;;; init.el ends here
