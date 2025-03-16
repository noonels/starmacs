;;; treemacs-config.el --- Treemacs configuration
;;
;;; Commentary:
;; Author: M Cooper Healy
;;
;;; Code:


(use-package treemacs
  :ensure t
  :bind
  (:map global-map
        ("s-\\"      . treemacs-select-window)
        ("s-b"       . treemacs)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag))
  :custom
  (treemacs-is-never-other-window  t))

(provide 'treemacs-config)
;;; treemacs-config.el ends here
