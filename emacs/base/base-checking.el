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
;; `flyspell' and `flycheck' to configure them.

;;; Code:

;;; `spelling-hydra':
;; A hydra for quickly moving through your buffer, moving from one error to the
;; other, checking and correcting them as you go. Also enables toggling of
;; either straight up `flyspell' or of it's `prog-mode'.
;;
;; Ever so lightly stolen from rmberYou
(defhydra hydra-spelling (:color blue)
  "
  ^
  ^Errors^            ^Checker^            ^Mode^
  ^──────^─────────── ^───────^─────────── ^────^─────────
  _k_: previous       _f_: check           _m_: mode
  _j_: next           _c_: correction      _p_: prog mode
  ^^                  _d_: dictionary      ^^
  ^^                  ^^                   ^^
  "
  ("q" nil "quit")
  ("k" flyspell-correct-previous :color pink)
  ("j" flyspell-correct-next :color pink)
  ("c" ispell)
  ("d" ispell-change-dictionary)
  ("f" flyspell-buffer)
  ("m" flyspell-mode)
  ("p" flyspell-prog-mode))

;;; `flyspell':
;; The builtin spell checker for Emacs, this is a really nice little package
;; that automatically does it's magic whenever it's needed. For programming
;; modes we use the builtin `prog-mode' version of Flyspell, and we then just
;; enable the regular version for `text-mode' buffers.
(use-package flyspell
  :commands (flyspell-mode flyspell-prog-mode)
  :delight " Ⓢ"
  :ghook ('prog-mode-hook #'flyspell-prog-mode)
  :ghook ('text-mode-hook #'flyspell-mode)
  :general
  (amalthea-leader
    "S s" '(hydra-spelling/body :wk "hydra")
    "S b" '(flyspell-buffer :wk "spell check buffer")
    "S n" '(flyspell-goto-next-error :wk "next spelling error"))
  :init
  (progn
    (csetq ispell-program-name "aspell"
           ispell-local-dictionary "en_US"
           flyspell-use-meta-tab nil
           flyspell-issue-message-flag nil
           flyspell-issue-welcome-flag nil)))

;;; `flyspell-correct':
;; The default correction window for Flyspell is awful, terribly so actually, so
;; we'll use a package to fix this. This creates a generic way of correcting
;; words and we'll use a Ivy-minibuffer to correct wording.
;; TODO Find a better way to get to the Ivy menu in Flyspell
(use-package flyspell-correct-ivy
  :after flyspell
  :commands (flyspell-correct-word-generic
             flyspell-correct-ivy
             flyspell-correct-previous)
  :general
  (amalthea-leader
    "S c" '(flyspell-correct-previous :wk "correct prev word")
    "S C" '(flyspell-correct-next :wk "correct next word")
    :init (csetq flyspell-correct-interface #'flyspell-correct-ivy)))

;;; `flycheck':
(use-package flycheck
  :delight " Ⓒ"
  :commands global-flycheck-mode
  :init (global-flycheck-mode))

(provide 'base-checking)
;;; base-checking.el ends here
