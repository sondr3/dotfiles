;;; base-checking.el --- Syntax and spell checking -*- lexical-binding: t -*-

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

;; Syntax and spell checking for both programming and regular languages, using
;; `flyspell' and `flycheck' to configure them. Requirements are that you need
;; to have `hunspell' installed for this to work, though `use-package' will try
;; to install it for you.

;;; Code:

;;; `flyspell':
;; The builtin spell checker for Emacs, this is a really nice little package
;; that automatically does it's magic whenever it's needed. For programming
;; modes we use the builtin `prog-mode' version of Flyspell, and we then just
;; enable the regular version for `text-mode' buffers.
(use-package flyspell
  :commands (flyspell-mode flyspell-prog-mode)
  :ensure-system-package hunspell
  :delight " Ⓢ"
  :general
  (amalthea-leader
    :keymaps 'normal
    "S" '(:ignore t :which-key "spelling")
    "S b" '(flyspell-buffer :which-key "spell check buffer")
    "S n" '(flyspell-goto-next-error :which-key "next spelling error"))
  :init
  (setenv "DICPATH" (concat (getenv "HOME") "/Library/Spelling"))
  (general-add-hook 'prog-mode-hook #'flyspell-prog-mode)
  (dolist (mode-hook '(text-mode-hook))
    (general-add-hook mode-hook #'flyspell-mode))
  (progn
    (setq ispell-program-name "hunspell"
          ispell-really-hunspell t
          ispell-local-dictionary "en_US"
          flyspell-use-meta-tab nil
          flyspell-issue-message-flag nil
          flyspell-issue-welcome-flag nil)))

;;; `flyspell-correct':
;; The default correction window for Flyspell is awful, terribly so actually, so
;; we'll use a package to fix this. This creates a generic way of correcting
;; words and we'll use a Ivy-minibuffer to correct wording.
(use-package flyspell-correct-ivy
  :after flyspell
  :commands (flyspell-correct-word-generic
             flyspell-correct-ivy
             flyspell-correct-previous-word-generic)
  :general
  (amalthea-leader
    :keymaps 'normal
    "S c" '(flyspell-correct-previous-word-generic :which-key "correct word"))
  :init (setq flyspell-correct-interface #'flyspell-correct-ivy))

(provide 'base-checking)
;;; base-checking.el ends here
