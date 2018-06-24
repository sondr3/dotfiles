;;; elisp.el --- Configuration for Elisp code -*- lexical-binding: t -*-

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

(use-package emacs-lisp
  :gfhook #'auto-compile-on-load-mode #'auto-compile-on-save-mode
  :ghook 
  ('emacs-lisp-mode-hook #'outline-minor-mode)
  ('emacs-lisp-mode-hook #'reveal-mode)
  :general
  (:keymaps 'emacs-lisp-mode-map
            "C-c e" 'macrostep-expand))

(use-package auto-compile
  :commands (auto-compile-on-load-mode auto-compile-on-save-mode)
  :custom
  (auto-compile-display-buffer nil "Don't automatically show the *Compile Log* buffer")
  (auto-compile-mode-line-counter t "Display number of warnings in modeline")
  (auto-compile-source-recreate-deletes-dest t "Delete leftover byte code when recompiling")
  (auto-compile-toggle-deletes-nonlib-dest t "Delete non-library byte code")
  (auto-compile-update-autoloads t "Update autoloads after compiling")
  :config
  (add-hook 'auto-compile-inhibit-compile-hook 'auto-compile-inhibit-compile-detached-git-head))

(use-package macrostep
  :custom
  (macrostep-expand-in-separate-buffer t "Show macro expansion in a new buffer"))

(provide 'elisp)
;;; elisp.el ends here
