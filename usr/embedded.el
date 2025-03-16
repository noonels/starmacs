;;; embedded.el --- Utility functions -*- lexical-binding: t; mmm-mode: nil; -*-
;;
;;; Commentary:
;; Author: M Cooper Healy <m.cooper.healy@gmail.com>
;;
;;; Code:

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(add-to-list 'auto-mode-alist '("\\.sqli\\'" . sql-mode))
(use-package mmm-mode
  :ensure t
  :custom
  (mmm-global-mode 'maybe)
  :config
  (set-face-background 'mmm-default-submode-face nil)
  (mmm-add-classes
   '((embedded-sql
      :submode sql-mode
      :face mmm-code-submode-face
      :front "\\(--SQL\\)"
      :front-offset (beginning-of-line)
      :back "\\(--SQL-END\\)"
      :back-offset (end-of-line))
     (embedded-html
      :submode html-mode
      :face mmm-code-submode-face
      :front "\\(<!-- HTML -->\\)"
      :front-offset (beginning-of-line)
      :back "\\(<!-- HTML-END -->\\)"
      :back-offset (end-of-line))
  (mmm-add-mode-ext-class nil nil 'embedded-sql)
  (mmm-add-mode-ext-class nil nil 'embedded-html))))

(provide 'utils)
;;; embedded.el ends here
