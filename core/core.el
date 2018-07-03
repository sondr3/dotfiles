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

;;; Variables

(defvar amalthea-init-time nil
  "Displays the time it took for Amalthea to launch.")

(defvar amalthea-post-init-hook nil
  "Hook that runs after Emacs has loaded.")

(defvar init-file-debug) ;; silence warning
(defvar amalthea-debug init-file-debug
  "Whether to log and enable debugging by defalt for Amalthea.")

;;; Macros


;;; TODO: Write use-package keyword instead, like `:custom' but for `:init' and
;;; `:config' instead
(defmacro setq-am (keyword value &optional _comment)
  "A thin macro that wraps `setq' that allows us to write
comments after the variable. Although this forces you to write it
a lot more instead of declaring all the variables inside a single
`setq' block, the byte compiler readily merges them all together."
  `(setq ,keyword ,value))

(defmacro setq-default-am (keyword value &optional _comment)
  "A thin macro that wraps `setq-default' that allows us to write
comments after the variable. Although this forces you to write it
a lot more instead of declaring all the variables inside a single
`setq-default' block, the byte compiler readily merges them all
together."
  `(setq-default ,keyword ,value))

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
              max-specpdl-size 10000)
;; Directories and files
(setq-default abbrev-file-name (concat amalthea-local-dir "abbrev.el")
              backup-directory-alist (list (cons "." (concat amalthea-cache-dir "backup/")))
              auto-save-list-file-name (concat amalthea-cache-dir "autosave")
              custom-file (concat amalthea-local-dir "custom.el"))
(setq-default-am byte-compile--use-old-handlers nil "Use the most recent byte code ops")
(setq-default-am create-lockfiles nil "Don't create #filename# files in directories")
(setq-default-am make-backup-files nil "Don't create backup files, we're using VC")
(setq-default-am sentence-end-double-space nil "Sentences end with a single space")
(setq-default-am vc-follow-symlinks t "Always follow symbolic links")
;; Save paste history when killing Emacs
(setq-default-am save-interprogram-paste-before-kill t "Save paste history when killing Emacs")

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
  (interactive)
  (dolist (dir (list amalthea-cache-dir amalthea-local-dir))
    (unless (file-directory-p dir)
      (make-directory dir t))))

(defun amalthea--byte-compile-core-dirs ()
  "TODO: There's probably a better way than this..."
  (interactive)
  (dolist (dir (list amalthea-core-dir amalthea-base-dir amalthea-utils-dir))
    (byte-recompile-directory dir 0 t))
  (dolist (file (list (concat amalthea-emacs-dir "init.el")
                      (concat amalthea-emacs-dir "early-init.el")))
    (byte-compile-file file)))

;;; Launch

;; Ensure directories exists before needing them and load the core configuration
(eval-and-compile
  (amalthea--ensure-core-dirs)
  (add-to-list 'load-path amalthea-core-dir))

(require 'core-os)
(require 'core-packages)
(require 'core-ui)
(require 'core-keybindings)
(require 'core-editor)

;;; Load the base configuration
(require 'base (concat amalthea-base-dir "base"))

;;; Load all the modules
(require 'modules (concat amalthea-modules-dir "modules"))

(provide 'core)
;;; core.el ends here
