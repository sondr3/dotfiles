;; MELPA packages for package goodness
(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(require 'ob-tangle)
(org-babel-load-file
 (expand-file-name "emacs-init.org"
		               user-emacs-directory))
