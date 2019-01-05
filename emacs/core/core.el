;;; core.el --- hello world -*- lexical-binding: t -*-

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

;; Emacs is nearly twice my age, and as such has a bunch of pretty funny
;; defaults that I deem necessary to change. Quite frankly, there's a lot of
;; them and I have probably missed a bunch of them too.

;;; Code:

(eval-when-compile
  (unless (not (version< emacs-version "26"))
    (error "You are using Emacs %s, Amalthea requires version 26 or higher" emacs-version)))

;;; Constants

(defconst amalthea-version "0.1.0"
  "Current version of Amalthea.")

(defconst amalthea-emacs-dir (eval-when-compile (file-truename user-emacs-directory))
  "Path to the current Emacs directory.")

(defconst amalthea-core-dir (expand-file-name "core/" amalthea-emacs-dir)
  "Core functionality and packages live in this directory.")

(defconst amalthea-base-dir (expand-file-name "base/" amalthea-emacs-dir)
  "Base functionality that isn't a core part of Amalthea.")

(defconst amalthea-modules-dir (expand-file-name "modules/" amalthea-emacs-dir)
  "Modules for languages, features and the world lives here.")

(defconst amalthea-utils-dir (expand-file-name "utils/" amalthea-emacs-dir)
  "Utility functions and packages for Amalthea.")

;;; Variables

(defvar amalthea-init-time nil
  "Displays the time it took for Amalthea to launch.")

(defvar amalthea-post-init-hook nil
  "Hook that runs after Emacs has loaded.")

(defvar init-file-debug) ;; silence warning
(defvar amalthea-debug init-file-debug
  "Whether to log and enable debugging by defalt for Amalthea.")

;;; Settings

;; Emacs actually predates UTF8, which to my mind is kinda nuts. So we'll force
;; Emacs to always use unicode characters and UTF8 everywhere.
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system                   'utf-8)
(set-terminal-coding-system             'utf-8)
(set-keyboard-coding-system             'utf-8)
(set-selection-coding-system            'utf-8)
(setq locale-coding-system              'utf-8)
(setq-default buffer-file-coding-system 'utf-8)

;; Then we can set some defaults that are core to Emacs. Beginning with changing
;; the initial screen for Emacs.
(setq-default inhibit-startup-message t
              inhibit-startup-buffer-menu t
              inhibit-startup-screen t
              inhibit-startup-echo-area-message t
              initial-buffer-choice t
              initial-major-mode 'fundamental-mode
              ;; Recusion and lisp call limit
              max-lisp-eval-depth 50000
              max-specpdl-size 10000
              ;; Set some common sense settings
              custom-file (expand-file-name "custom.el" amalthea-emacs-dir)
              byte-compile--use-old-handlers nil     ;; Use the most recent byte code ops
              sentence-end-double-space nil          ;; Sentences end with a single space
              vc-follow-symlinks t                   ;; Always follow symbolic links
              save-interprogram-paste-before-kill t) ;; Save paste history when killing Emacs)

;; Fully inhibit the initial screen
(fset #'display-startup-echo-area-message #'ignore)

;; Mostly to save at most two strokes and at a minimum one. Efficiency baby.
(fset #'yes-or-no-p #'y-or-n-p)

;; Only load `custom.el' if it exists
(when (file-exists-p custom-file)
  (load custom-file t t))

;;; Functions
(defun amalthea--byte-compile-amalthea ()
  "Byte compile all files and directories used in Amalthea."
  (interactive)
  (dolist (file (list (expand-file-name "early-init.el" amalthea-emacs-dir)
                      (expand-file-name "init.el" amalthea-emacs-dir)))
    (byte-compile-file file))
  (dolist (dir (list amalthea-core-dir amalthea-base-dir amalthea-utils-dir amalthea-modules-dir))
    (byte-recompile-directory dir 0 t)))

;;; Launch

;; Ensure directories exists before needing them and load the core configuration
(eval-and-compile
  (add-to-list 'load-path amalthea-core-dir t))

(require 'core-packages)
(require 'core-os)
(require 'core-ui)
(require 'core-keybindings)
(require 'core-editor)

;;; Load the base configuration
(require 'base (expand-file-name "base" amalthea-base-dir))

;;; Load all the modules
(require 'modules (expand-file-name "modules" amalthea-modules-dir))

;; Load the utilities
(require 'utils (expand-file-name "utils" amalthea-utils-dir))

(provide 'core)
;;; core.el ends here
