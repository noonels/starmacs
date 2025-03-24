;;; theme.el --- Custom theme settings
;;
;;; Commentary:
;; Author: M Cooper Healy
;;
;;; Code:

;; Got bit by the Atom bug again (RIP), so I'm gonna give Anisochromatic a break for a while.
(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (doom-themes-org-config))
(load-theme 'doom-one t)

 (use-package doom-modeline
   :ensure t
   :hook (after-init . doom-modeline-mode)
   :config
   (setq doom-modeline-height 35)
   (doom-modeline-def-modeline 'main
     '(bar matches buffer-info remote-host buffer-position parrot selection-info)
     '(misc-info minor-modes checker input-method buffer-encoding major-mode process vcs "  ")))

(use-package rainbow-delimiters
    :ensure t
    :hook
    (prog-mode . rainbow-delimiters-mode)
    (org-mode . rainbow-delimiters-mode))

;; Show line numbers everywhere, except disabled modes
(column-number-mode)
(global-display-line-numbers-mode t)

;; Add disabled modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(provide 'theme)
;;; theme.el ends here
