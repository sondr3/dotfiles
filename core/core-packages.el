;;; core-packages.el --- Package configuration -*- lexical-binding: t -*-

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

;; There's a ton of different ways of doing package management in Emacs, most of
;; which nowadays revolve around `use-package'. There's also `quelpa',
;; `straight.el', and some even resort to manually listing all the packages they
;; want and then installing them that way. I've tried a bunch of them but in the
;; end I really fell for `Borg' because of it's simplicity but also because it
;; handles reproducability really well since every package is a git submodule.

;; It also makes it really easy to integrate it with `use-package' that too
;; brings with it all the goodies that `use-package' brings, like lazy loading,
;; easy configuring of hooks and settings and so on.

;; Then, and this isn't required per se, but I'll be using Delight to hide or
;; rename the names of major and minor modes on the modeline instead of
;; Diminish. Mostly because when I last checked it didn't support hiding major
;; modes or renaming them without weird hacks. And now that `use-package' has
;; built-in support for the `:delight' keyword, I don't really need it.

;;; Code:

(add-to-list 'load-path (expand-file-name "lib/borg" amalthea-emacs-dir))
(require 'borg)
(borg-initialize)
(require 'use-package)
(setq use-package-verbose t
      use-package-compute-statistics t
      use-package-always-defer t)

(use-package epkg
  :init (setq epkg-repository (expand-file-name "epkgs/" amalthea-cache-dir)))

(use-package delight)

(provide 'core-packages)

;;; core-packages.el ends here
