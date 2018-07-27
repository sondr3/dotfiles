;;; base-git.el --- Git -*- lexical-binding: t -*-

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

;; Like pretty much everybody nowadays I'm using ~git~, and with that comes
;; probably one of the absolutely best packages that exists for Emacs: Magit!
;; Even if you're okay at using git from the command line, Magit just blows the
;; command line interface for git out of the water. If you haven't tried it I
;; highly recommend it.

;;; Code:

;;; `git-hydra':
;; Quickly move between hunks in your document.
(defhydra hydra-git (:color pink)
  "git"
  ("k" diff-hl-previous-hunk "prev hunk")
  ("j" diff-hl-next-hunk "next hunk")
  ("q" nil "quit" :color blue))

;;; `Magit':
;; Enable and appreciate it! The only thing we'll really change is adding a few
;; extra functions and hooks to work better with Borg.
(use-package magit
  :delight auto-revert-mode
  :commands magit-add-section-hook
  :general
  (amalthea-leader
    :keymaps 'normal
    "g h" '(hydra-git/body :which-key "hydra")
    "g s" '(magit-status :which-key "git status"))
  :config
  (progn
    (magit-add-section-hook 'magit-status-sections-hook
                            'magit-insert-modules
                            'magit-insert-stashes
                            'append)))

;;; `git-modes':
;; A few minor major modes for editing `.gitignore', `.gitattributes' and
;; `.gitconfig' files.
(use-package git-modes)

;;; `evil-magit':
;; Magit by default doesn't include any Evil keybindings, which makes sense but
;; is kinda required since we use Evil.
(use-package evil-magit
  :after magit
  :commands evil-magit-init
  :init (evil-magit-init))

;;; `diff-hl':
;; There's a plugin for Vim called GitGutter that is really neat, in the fringe
;; of your file it shows where hunks have been changed, added and removed from
;; the file. There's a similarly named plugin for Emacs, but it hasn't been
;; updated for quite a while and even then, `diff-hl' is quite a lot better than
;; it is. There's no magic here, we'll enable it globally, hook into Magit so
;; that diff-hl updates when we commit using Magit.
(use-package diff-hl
  :commands (diff-hl-magit-post-refresh global-diff-hl-mode)
  :functions (diff-hl-flydiff-mode diff-hl-margin-mode)
  :defines diff-hl-margin-symbols-alist
  :general
  (amalthea-leader
    :keymaps 'normal
    "g j" '(diff-hl-next-hunk :which-key "next hunk")
    "g k" '(diff-hl-previous-hunk :which-key "previous hunk"))
  :init
  (progn
    (setq diff-hl-margin-symbols-alist
          '((insert . "+") (delete . "-") (change . "~")
            (unknown . "?") (ignored . "i")))
    (global-diff-hl-mode)
    (diff-hl-margin-mode)
    (diff-hl-flydiff-mode)
    (general-add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)))

(provide 'base-git)

;;; base-git.el ends here
