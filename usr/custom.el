;;; custom.el --- All of my look-and-feel configuration
;;
;;; Commentary:
;; Author: M Cooper Healy
;;
;;; Code:

(setq all-the-icons-scale-factor 1.1)
(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room


(toggle-frame-maximized)    ; Always start maximized
(pixel-scroll-precision-mode 1) ; pixel-perfect scrolling
(setq-default cursor-type '(bar . 2)) ; blinking bar
(set-default 'truncate-lines t) ; Truncate, don't wrap
;; (define-fringe-bitmap 'right-arrow
;;   [#b00000000
;;     #b00011000
;;     #b00111100
;;     #b01111110
;;     #b01111110
;;     #b00111100
;;     #b00011000
;;     #b00000000])
;; (define-fringe-bitmap 'left-arrow
;;   [#b00000000
;;     #b00011000
;;     #b00111100
;;     #b01111110
;;     #b01111110
;;     #b00111100
;;     #b00011000
;;     #b00000000])


; focus-line
(if (window-system)
    (global-hl-line-mode 1))

; TODO highlighting
(use-package hl-todo
  :ensure t
  :config
  (global-hl-todo-mode 1))

; Whitespace should be visible, and memorable
(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq-default tab-width 4)
(setq whitespace-style (quote (face tabs newline tab-mark newline-mark)))

(setq whitespace-display-mappings
      '((newline-mark 10 [172 10])
        (tab-mark 9 [187 9] [92 9])))
(global-whitespace-mode 't)

(custom-set-faces
  '(whitespace-tab((t (:foreground "#525252"))))
  '(whitespace-newline((t (:foreground "#525252")))))

(xterm-mouse-mode 1)         ; mouse in terminal
(delete-selection-mode 1)    ; replace selected text when typing
(global-auto-revert-mode t)  ; When file changes, update buffer
(setq make-backup-files nil) ; Don't save backup~ files
(setq auto-save-default nil) ; Don't save #autosave# files


;; Add path to extra binaries
(use-package exec-path-from-shell
    :config
    (exec-path-from-shell-initialize))

(provide 'custom)
;;; custom.el ends here
