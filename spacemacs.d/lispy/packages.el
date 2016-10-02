;;; packages.el --- lispy layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sondre Nilsen <nilsen.sondre@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;;; Code:

(defconst lispy-packages
  '(lispy
    lispyville))

(defun lispy/init-lispy ()
  (use-package lispy
    :defer t
    :init
    (progn
      (add-hook 'emacs-lisp-mode-hook #'lispy-mode)
      (add-hook 'scheme-mode-hook #'lispy-mode))
    :config
    (progn
      (spacemacs|diminish lispy-mode " Ⓛ" " L"))))

(defun lispy/init-lispyville ()
    (use-package lispyville
      :defer t
      :config
      (progn
        (spacemacs|diminish lispyville-mode " ")
        (add-hook 'lispy-mode-hook #'lispyville-mode))))

;;; packages.el ends here
