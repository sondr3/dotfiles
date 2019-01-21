;;; +bindings.el --- Evil and other keybindings for Org -*- lexical-binding: t -*-

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

;;; vim-like confirm/abort for capture and src
;;; Taken from mwillsey (Max Willsey) on https://github.com/syl20bnr/spacemacs/pull/7400
(with-eval-after-load 'org-capture
  (define-key org-capture-mode-map [remap evil-save-and-close]          'org-capture-finalize)
  (define-key org-capture-mode-map [remap evil-save-modified-and-close] 'org-capture-finalize)
  (define-key org-capture-mode-map [remap evil-quit]                    'org-capture-kill))

(with-eval-after-load 'org-src
  (define-key org-src-mode-map [remap evil-save-and-close]              'org-edit-src-exit)
  (define-key org-src-mode-map [remap evil-save-modified-and-close]     'org-edit-src-exit)
  (define-key org-src-mode-map [remap evil-quit]                        'org-edit-src-abort))

(with-eval-after-load 'org-table
  (define-key org-table-fedit-map [remap evil-save-and-close]           'org-table-fedit-finish)
  (define-key org-table-fedit-map [remap evil-save-modified-and-close]  'org-table-fedit-finish)
  (define-key org-table-fedit-map [remap evil-quit]                     'org-table-fedit-abort))

(provide '+bindings)

;;; +bindings.el ends here
