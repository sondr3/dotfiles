;;; lang-web.el --- CSS, HTML++ for Amalthea -*- lexical-binding: t -*-

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

;; This is a work in progress because working with anything on the front-end is
;; seriously painful.

;;; Code:

(use-package rainbow-mode
  :delight)

(use-package emmet-mode
  :delight
  :general
  (amalthea-major-leader 'emmet-mode-map
    "w" '(emmet-wrap-with-markup :wk "wrap region")
    "e" '(emmet-expand-yas :wk "yas expand"))
  :ghook ((list 'css-mode-hook 'scss-mode-hook 'html-mode-hook) #'emmet-mode))

(use-package css-mode
  :ghook ('css-mode-hook (list #'rainbow-delimiters-mode #'rainbow-mode)))

(use-package counsel-css
  :general
  (amalthea-major-leader 'css-mode-map
    "c" '(counsel-css :wk "counsel"))
  :ghook ('css-mode-hook #'counsel-css-imenu-setup))

(use-package prettier-js
  :ghook ((list 'css-mode-hook 'scss-mode-hook 'html-mode-hook 'js2-mode-hook 'web-mode-hook) #'prettier-js-mode))

(provide 'lang-web)
;;; lang-web.el ends here
