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
(require 'magit)
(with-eval-after-load 'magit
  (delight 'auto-revert-mode nil "magit")
  (amalthea-leader
    "g h" '(hydra-git/body :wk "hydra")
    "g s" '(magit-status :wk "git status")))

;;; `git-modes':
;; A few minor major modes for editing `.gitignore', `.gitattributes' and
;; `.gitconfig' files.
(require 'gitignore-mode)
(require 'gitattributes-mode)
(require 'gitconfig-mode)

;;; `evil-magit':
;; Magit by default doesn't include any Evil keybindings, which makes sense but
;; is kinda required since we use Evil.
(require 'evil-magit)
(with-eval-after-load 'magit
  (evil-magit-init))

;;; `diff-hl':
;; There's a plugin for Vim called GitGutter that is really neat, in the fringe
;; of your file it shows where hunks have been changed, added and removed from
;; the file. There's a similarly named plugin for Emacs, but it hasn't been
;; updated for quite a while and even then, `diff-hl' is quite a lot better than
;; it is. There's no magic here, we'll enable it globally, hook into Magit so
;; that diff-hl updates when we commit using Magit.
(setq diff-hl-margin-symbols-alist
      '((insert . "+") (delete . "-") (change . "~")
        (unknown . "?") (ignored . "i")))
(require 'diff-hl)
(with-eval-after-load 'diff-hl
  (general-add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)
  (amalthea-leader
    "g j" '(diff-hl-next-hunk :wk "next hunk")
    "g k" '(diff-hl-previous-hunk :wk "previous hunk"))
  (global-diff-hl-mode)
  (diff-hl-margin-mode)
  (diff-hl-flydiff-mode))

;;; `hl-todo':
;; This is a really simple mode that highlights things that are marked as TODO,
;; FIXME and so on. It's quite useful if you like to litter your project with
;; them.
(require 'hl-todo)
(global-hl-todo-mode)

(provide 'base-git)
;;; base-git.el ends here
