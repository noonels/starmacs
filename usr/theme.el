;;; theme.el --- Custom theme settings
;;
;;; Commentary:
;; Author: M Cooper Healy
;;
;;; Code:
(use-package autothemer
    :ensure t)

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

;; (use-package base-line
;;   :vc (:url "http://github.com/isomatter-labs/base-line" :files ("dist" "*.el"))
;;   :ensure t
;;   :hook (after-init . base-line-mode))

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

;; Visible bell
; (setq visible-bell t)
; (setq ring-bell-function
;      (lambda ()
;        (let ((orig-bg (face-background 'mode-line-active)))
;          (set-face-background 'mode-line-active "#ef8e49")
;          (starmacs/set-modeline-box-bg 'mode-line-active "#ef8e49")
;          (run-with-idle-timer 0.1 nil
;                                (lambda (bg) (progn (set-face-background 'mode-line-active bg)
;                                                    (starmacs/set-modeline-box-bg 'mode-line-active bg)))
;                                orig-bg))))

(provide 'theme)
;;; theme.el ends here
