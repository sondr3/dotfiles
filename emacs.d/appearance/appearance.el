;; Smart stuff
(setq visible-bell t
      font-lock-maximum-decoration t
      color-theme-is-global t
      truncate-partial-width-windows nil)

;; show column number and line number
(require 'linum-relative)
(setq linum-relative-format "%3s ")
(dolist (mode '(column-number-mode line-number-mode))
  (when (fboundp mode) (funcall mode t)))
(dolist (mode-hook '(text-mode-hook prog-mode-hook conf-mode-hook))
  (add-hook mode-hook
	    (lambda ()
	      (linum-mode 1))))

;; make the fringe thinner
(fringe-mode 4)

;; show matching parenthesis'
(show-paren-mode 1)
(setq show-paren-style 'expression)
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (blink-cursor-mode -1))

;; highlight the line you're on
(global-hl-line-mode 1)

;; Something smart
(setq redisplay-dont-pause t)

(provide 'appearance)
