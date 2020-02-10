;;; am-core.el --- Amalthea core -*- lexical-binding: t -*-

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

;; Core configuration.

;;; Code:
;;; Amalthea group and customizations

(defgroup amalthea nil
  "Amalthea settings and configurations."
  :group 'convenience
  :prefix "amalthea")

;;;; Directories
(defcustom amalthea-emacs-dir (eval-when-compile (file-truename user-emacs-directory))
  "Path to the current Emacs directory."
  :type 'directory
  :group 'amalthea)

(defcustom amalthea-core-dir (expand-file-name "core/" amalthea-emacs-dir)
  "Core packages and configuration for Amalthea."
  :type 'directory
  :group 'amalthea)

(defcustom amalthea-module-dir (expand-file-name "modules/" amalthea-emacs-dir)
  "Modules directory for Amalthea."
  :type 'directory
  :group 'amalthea)

(defcustom amalthea-util-dir (expand-file-name "utils/" amalthea-emacs-dir)
  "Utility directory for Amalthea."
  :type 'directory
  :group 'amalthea)

(defcustom amalthea-org-dir (expand-file-name "org/" amalthea-emacs-dir)
  "Org directory for Amalthea."
  :type 'directory
  :group 'amalthea)

(defcustom amalthea-dotfiles-dir (expand-file-name ".dotfiles" (getenv "HOME"))
  "Location of dotfiles for Amalthea."
  :type 'directory
  :group 'amalthea)

;;;; Keybindings
(defcustom amalthea-leader-key "SPC"
  "The default leader key for Amalthea."
  :type 'string
  :group 'amalthea)

(defcustom amalthea-leader-secondary-key "C-SPC"
  "The secondary leader key for Amalthea."
  :type 'string
  :group 'amalthea)

(defcustom amalthea-major-leader-key ","
  "The default major mode leader key for Amalthea."
  :type 'string
  :group 'amalthea)

(defcustom amalthea-major-leader-secondary-key "M-,"
  "The secondary major mode leader key for Amalthea."
  :type 'string
  :group 'amalthea)

;;;; Assorted
(defcustom amalthea-mono-font "PragmataPro Mono Liga"
  "The default monospaced font that Amalthea uses."
  :type 'string
  :group 'amalthea)

(defcustom amalthea-serif-font "PragmataPro Liga"
  "The default sans serif font that Amalthea uses."
  :type 'string
  :group 'amalthea)

(defcustom amalthea-line-spacing 0.15
  "The default line spacing width that Amalthea uses."
  :type 'number
  :group 'amalthea)

(defcustom amalthea-font-size 140
  "The default font size for Amalthea."
  :type 'number
  :group 'amalthea)

;;; Core utilities

(defun amalthea--byte-compile-amalthea ()
  "Byte compile all files and directories used in Amalthea."
  (interactive)
  (dolist (file (list (expand-file-name "early-init.el" amalthea-emacs-dir)
                      (expand-file-name "init.el" amalthea-emacs-dir)))
    (byte-compile-file file))
  (dolist (dir (list amalthea-core-dir amalthea-module-dir amalthea-util-dir amalthea-org-dir))
    (byte-recompile-directory dir 0 t)))

(defmacro csetq (&rest body)
  "A `setq' macro that works with `custom-set' properties. The
BODY is a list of the variables to be set."
  `(progn
     ,@(cl-loop for (var val) on body by 'cddr
                collect `(funcall (or (get ',var 'custom-set) #'set)
                                  ',var ,val))))

;;; Core settings
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

;; Hide the backup files inside `no-littering' directories.
(csetq backup-by-copying t            ;; Don't clobber symlinks
       delete-old-versions t          ;; Silently delete old files
       delete-by-moving-to-trash t    ;; And move them into the trash can
       kept-new-versions 6            ;; Keep some new versions around
       kept-old-versions 2            ;; But just a few old ones
       version-control t              ;; use versioned backups
       auto-save-file-name-transforms
       `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)) ;; Store auto save files in `no-littering'
       backup-directory-alist
       `((".*" . ,(no-littering-expand-var-file-name "backup/")))    ;; Store backups in `no-littering'
       ad-redefinition-action 'accept) ;; Don't give warnings for things out of my control

;; Fully inhibit the initial screen
(fset #'display-startup-echo-area-message #'ignore)

;; Mostly to save at most two strokes and at a minimum one. Efficiency baby.
(fset #'yes-or-no-p #'y-or-n-p)

;; Only load `custom.el' if it exists
(when (file-exists-p custom-file)
  (load custom-file t t))

;;; Load core configuration
(add-to-list 'load-path amalthea-core-dir t)

(require 'am-packages)
(require 'am-keybindings)
(require 'am-system)
(require 'am-evil)
(require 'am-ui)
(require 'am-editor)
(require 'am-completion)
(require 'am-git)
(require 'am-projects)
(require 'am-checking)

;;; Load the rest of the configuration
;;;; Load all the modules
(require 'am-modules (expand-file-name "am-modules" amalthea-module-dir))

;;;; Load the org configuration
(require 'am-org (expand-file-name "am-org" amalthea-org-dir))

;;;; Load the utilities
(require 'am-utils (expand-file-name "am-utils" amalthea-util-dir))


(provide 'am-core)

;;; am-core.el ends here
