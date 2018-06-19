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
  (unless (>= emacs-major-version 26) 
    (error "You are using Emacs %s, Amalthea requires version 26 or higher" emacs-version)))

;;; Constants

(defconst amalthea-version "0.1.0"
  "Current version of Amalthea.")

(defconst amalthea-emacs-dir (eval-when-compile (file-truename user-emacs-directory))
  "Path to the current Emacs directory.")

(defconst amalthea-core-dir (concat amalthea-emacs-dir "core/")
  "Core functionality and packages live in this directory.")

(defconst amalthea-base-dir (concat amalthea-emacs-dir "base/")
  "Base functionality that isn't a core part of Amalthea.")

(defconst amalthea-modules-dir (concat amalthea-emacs-dir "modules/")
  "Modules for languages, features and the world lives here.")

(defconst amalthea-utils-dir (concat amalthea-emacs-dir "utils/")
  "Utility functions and packages for Amalthea.")

(defconst amalthea-local-dir (concat amalthea-emacs-dir "local/")
  "Storage for non-volatile files: custom.el, spelling updates etc.")

(defconst amalthea-cache-dir (concat amalthea-emacs-dir "cache/")
  "Storage for volatile files: caches, logs etc.")

(defgroup amalthea nil
  "Amalthea"
  :tag 'amalthea
  :group 'emacs)

;;; Variables

(defvar amalthea-init-time nil
  "Displays the time it took for Amalthea to launch.")

(defvar amalthea-post-init-hook nil
  "Hook that runs after Emacs has loaded.")

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
              ;; Use the most recent byte code ops
              byte-compile--use-old-handlers nil
              ;; Recusion and lisp call limit
              max-lisp-eval-depth 50000
              max-specpdl-size 10000
              ;; Backups
              create-lockfiles nil
              make-backup-files nil
              ;; Common sense
              sentence-end-double-space nil
              vc-follow-symlinks t
	            ;; Save paste history when killing Emacs
              save-interprogram-paste-before-kill t
              ;; Directories and files
              abbrev-file-name (concat amalthea-local-dir "abbrev.el")
              backup-directory-alist (list (cons "." (concat amalthea-cache-dir "backup/")))
              auto-save-list-file-name (concat amalthea-cache-dir "autosave")
	      custom-file (concat amalthea-local-dir "custom.el"))
;; Fully inhibit the initial screen
(fset #'display-startup-echo-area-message #'ignore)

;; Mostly to save at most two strokes and at a minimum one. Efficiency baby.
(fset #'yes-or-no-p #'y-or-n-p)

;; Only load `custom.el' if it exists
(when (file-exists-p custom-file)
  (load custom-file t t))

;;; Functions

(defun amalthea--ensure-core-dirs ()
  "Ensures that all the required directories for Amalthea are created."
  (dolist (dir (list amalthea-cache-dir amalthea-local-dir))
    (unless (file-directory-p dir)
      (make-directory dir t))))

;;; Launch

;; Ensure directories exists before needing them
(eval-when-compile (amalthea--ensure-core-dirs))

;;; Load the core configuration
(add-to-list 'load-path amalthea-core-dir)

(require 'core-os)
(require 'core-packages)
(require 'core-ui)
(require 'core-keybindings)
(require 'core-editor)

;;; Load the base configuration
(require 'base (concat amalthea-emacs-dir "base/base"))

(provide 'core)

;;; core.el ends here
