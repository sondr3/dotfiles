;; hide some stuff
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; show column number and line number
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

;; highlight the line you're on
(global-hl-line-mode 1)

;; font settings
(set-face-attribute 'default nil :family "Monaco")

(provide 'appearance)
