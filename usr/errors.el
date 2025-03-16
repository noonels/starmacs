;;; errors.el --- Settings for error-checking
;;
;;; Commentary:
;; Author: M Cooper Healy
;;
;;; Code:

;; Treesitter
  (require 'treesit)
  (setq treesit-font-lock-level 4)
  (setq treesit-language-source-alist
  	  '((bash "https://github.com/tree-sitter/tree-sitter-bash")
  		(cmake "https://github.com/uyha/tree-sitter-cmake")
  		(css "https://github.com/tree-sitter/tree-sitter-css")
  		(elisp "https://github.com/Wilfred/tree-sitter-elisp")
  		(go "https://github.com/tree-sitter/tree-sitter-go")
  		(html "https://github.com/tree-sitter/tree-sitter-html")
  		(javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
  		(json "https://github.com/tree-sitter/tree-sitter-json")
  		(make "https://github.com/alemuller/tree-sitter-make")
  		(markdown "https://github.com/ikatyang/tree-sitter-markdown")
  		(python "https://github.com/tree-sitter/tree-sitter-python")
  		(toml "https://github.com/tree-sitter/tree-sitter-toml")
  		(tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
  		(typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
  		(yaml "https://github.com/ikatyang/tree-sitter-yaml")))

  (defun starmacs/install-grammers ()
    "Download and compile all of the grammers in treesit-language"
    (interactive)
    (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist)))

  (if (not (file-exists-p (expand-file-name "tree-sitter" user-emacs-directory)))
  	(starmacs/install-grammers))

  ;; (use-package treesit-auto
  ;;   :config
  ;;   (treesit-auto-add-to-auto-mode-alist 'all)
  ;;   (global-treesit-auto-mode))

;; FlyCheck
  (use-package flycheck
    :ensure t
    :custom (flycheck-check-syntax-automatically '(save mode-enabled))
    :init (global-flycheck-mode))

  (defvar-local starmacs--mode-line-flycheck "")

  (defun starmacs/mode-line-update-flycheck (&rest _)
    (setq starmacs--mode-line-flycheck
          (if (bound-and-true-p flycheck-mode)
              (concat
               "  "
               (pcase flycheck-last-status-change
                 (`not-checked (propertize "-/-" 'help-echo "Flycheck: not checked"))
                 (`no-checker (propertize "-" 'help-echo "Flycheck: no checker"))
                 (`running (propertize "*/*" 'help-echo "Flycheck: checking"))
                 (`errored (propertize "!" 'help-echo "Flycheck: error"))
                 (`finished
                  (let-alist (flycheck-count-errors flycheck-current-errors)
                    (propertize (format "%s/%s" (or .error 0) (or .warning 0))
                                'help-echo (if (or .error .warning)
                                               (concat "Flycheck: "
                                                       (when .error (format "%d errors%s" .error (if .warning ", " "")))
                                                       (when .warning (format "%d warnings" .warning))
                                                       "\nmouse-1: list errors")
                                             "Flycheck: no errors or warnings")
                                'local-map 'flycheck-error-list-mode-line-map)))
                 (`interrupted (propertize "x" 'help-echo "Flycheck: interrupted"))
                 (`suspicious (propertize "?" 'help-echo "Flycheck: suspicious"))))
            "")))

  (add-hook 'flycheck-status-changed-functions #'starmacs/mode-line-update-flycheck)
  (add-hook 'flycheck-mode-hook #'starmacs/mode-line-update-flycheck)

(provide 'errors)
;;; errors.el ends here