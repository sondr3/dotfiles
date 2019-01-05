;;; tool-docker.el --- Docker support -*- lexical-binding: t -*-

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

;; Docker configurations.

;;; Code:

;;; `dockerfile-mode':
;; Gives you syntax highlighting and completion for Docker.
(use-package dockerfile-mode
  :commands dockerfile-mode
  :general
  (amalthea-major-leader dockerfile-mode-map
    "b" 'dockerfile-build-buffer
    "B" 'dockerfile-build-no-cache-buffer)
  :mode "\\Dockerfile\\'")

(provide 'tool-docker)
;;; tool-docker.el ends here
