;;; org-config.el --- My configuration for Org-Mode
;;
;;; Commentary:
;; Author: M Cooper Healy
;;
;;; Code:

  (use-package org
    :demand t
    :hook
    (org-mode . visual-line-mode)
    (org-mode . variable-pitch-mode)
    (org-mode . (lambda () (indent-tabs-mode -1)))
    (org-mode . (lambda () (set-face-attribute 'org-block nil :foreground nil :font starmacs/fixed-pitch-font :height 120 :inherit 'fixed-pitch)))

    :custom
    (org-startup-with-inline-images t)
    (org-hide-emphasis-markers t)
    (org-pretty-entities t)

    :config
    (dolist (face '((org-level-1 . 1.30)
                    (org-level-2 . 1.20)
                    (org-level-3 . 1.10)
                    (org-level-4 . 1.05)
                    (org-level-5 . 1.05)
                    (org-level-6 . 1.05)
                    (org-level-7 . 1.05)
                    (org-level-8 . 1.05)))
      (set-face-attribute (car face) nil :font starmacs/title-font :weight 'thin :height (cdr face)))

    (set-face-attribute 'org-document-title nil :font starmacs/title-font :height 1.50 :weight 'regular)
    (set-face-attribute 'org-document-info nil :font starmacs/title-font :inherit '(shadow) :height 1.20 :weight 'thin)

    (set-face-attribute 'org-block nil :foreground nil :font starmacs/fixed-pitch-font :height 120 :inherit 'fixed-pitch)
    (set-face-attribute 'org-code nil   :font starmacs/fixed-pitch-font :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-table nil   :font starmacs/fixed-pitch-font :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-verbatim nil :font starmacs/fixed-pitch-font :inherit '(shadow fixed-pitch)))

(use-package org-roam
    :ensure t
    :init
    (setq org-roam-v2-ack t)
    :custom
    (org-roam-directory "~/Zettelkasten")
    (org-roam-completion-everywhere t)
    :bind (("C-c n l" . org-roam-buffer-toggle)
           ("C-c n f" . org-roam-node-find)
           ("C-c n i" . org-roam-node-insert)
           ("C-c n c" . org-roam-capture)
           :map org-mode-map
           ("C-M-i"    . completion-at-point))
    :config
    (require 'org-fold) ; Required to ensure the library loads for reasons I cannot yet fathom
    (org-roam-setup))

    (provide 'org-config)
    ;;; org-config.el ends here