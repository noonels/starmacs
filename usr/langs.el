;;; langs.el --- All of my language-specific configuration
;;
;;; Commentary:
;; Author: M Cooper Healy
;;
;;; Code:

;;; Python
(use-package python-black
  :demand t
  :after python
  :hook (python-ts-mode . python-black-on-save-mode-enable-dwim))

;;; TypeScript
(use-package web-mode
  :ensure t)

(define-derived-mode typescriptreact-mode web-mode "TypescriptReact"
  "A major mode for tsx.")

(use-package apheleia
  :config
  (setf (alist-get 'prettier apheleia-formatters)
        '(npx "prettier"
              "--trailing-comma"  "all"
              "--arrow-parens"    "always"
              "--tab-width"       "2"
              "--single-quote"    "true"
              "--semi"            "true"
              "--use-tabs"        "false"
              file))
  (setf (alist-get 'clj-zprint apheleia-formatters)
        '("clj-zprint"
          "{:style [:community :justified] :map {:comma? false}} <"
          file))
  (add-to-list 'apheleia-mode-alist '(typescriptreact-mode . prettier))
  (add-to-list 'apheleia-mode-alist '(typescript-mode . prettier))
  (add-to-list 'apheleia-mode-alist '(typescript-ts-mode . prettier))
  (add-to-list 'apheleia-mode-alist '(web-mode . prettier))
  (add-to-list 'apheleia-mode-alist '(clojure-mode . clj-zprint))
  (apheleia-global-mode t))


(use-package add-node-modules-path
  :ensure t
  :hook
  (typescript-ts-mode . add-node-modules-path)
  (typescriptreact-mode . add-node-modules-path)
  (typescript-mode . add-node-modules-path))

(use-package typescript-mode
  :mode (("\\.ts\\'" . typescript-mode)
         ("\\.tsx\\'" . typescriptreact-mode))
  :hook
  (typescript-mode . (lambda () (setq tab-width 2)))
  (typescriptreact-mode . (lambda () (setq tab-width 2)))
  (typescript-ts-mode . (lambda () (setq tab-width 2)))
  :custom
  (typescript-indent-level 2))

    (use-package eglot
    :ensure t
    :defer 3
    :bind
    (("s-." . eglot-code-actions)
    ("<f12>" . eglot-find-typeDefinition)
    ("<f2>" . eglot-rename))
    :hook
    (js-mode . eglot-ensure)
    (typescriptreact-mode . eglot-ensure)
    (typescript-mode . eglot-ensure)
    (typescript-ts-mode . eglot-ensure)
    (tsx-ts-mode . eglot-ensure)
    (python-ts-mode . eglot-ensure)
    (go-ts-mode . eglot-ensure)
    (f90-mode . eglot-ensure)
    (zig-mode . eglot-ensure)
    :config
      (setq lsp-prefer-flymake nil))

  (use-package eldoc-box
    :hook
    (eglot-managed-mode . eldoc-box-hover-mode))

;;; Julia
(use-package vterm :ensure t)
(use-package julia-repl)
(use-package eglot-jl)
(use-package julia-mode
  :mode "\\.jl\\'"
  :interpreter ("julia" . julia-mode)
  :init (setenv "JULIA_NUM_THREADS" "6")
  :config
  (add-hook 'julia-mode-hook 'julia-repl-mode)
  (add-hook 'julia-mode-hook 'eglot-jl-init)
  (add-hook 'julia-mode-hook 'eglot-ensure)
  (add-hook 'julia-mode-hook (lambda () (setq julia-repl-set-terminal-backend 'vterm))))

(setq eglot-jl-julia-command "/usr/local/bin/julia")
(setq julia-repl-executable-records
      '((default "/usr/local/bin/julia")
        (master "/usr/local/bin/julia")))

(setq-default indent-tabs-mode nil)

;;; Go
(use-package go-mode
  :ensure t
  :hook
  (go-mode . eglot-ensure)
  (go-mode . tree-sitter-hl-mode)
  :config
  (setq go-ts-indent-level 4))

(use-package zig-mode
  :ensure t
  :mode ("\\.zig\\'")
  :hook
  (zig-mode . eglot-ensure)
  (zig-mode . tree-sitter-hl-mode))

;;; LaTeX
(use-package tex
  :defer t
  :straight auctex
  :ensure auctex
  :config
  (setq TeX-auto-save t)
  (setq-default Tex-engine 'xetex)
  (setq-default TeX-PDF-mode t))

(use-package preview-latex
  :defer t
  :straight auctex
  :ensure auctex)

;;; Protobuf
(use-package protobuf-mode
  :straight t)

;;; Godot
(use-package gdscript-mode
  :straight (gdscript-mode
             :type git
             :host github
             :repo "godotengine/emacs-gdscript-mode"))

;;; Fortran 90+
; (add-to-list 'eglot-server-programs '(f90-mode . ("fortls" "--notify_init" "--nthreads=4")))

;;; LISP
;; no tabs in lisp
(add-hook 'emacs-lisp-mode-hook (lambda () (setq-local indent-tabs-mode nil)))
(add-hook 'lisp-mode-hook (lambda () (setq-local indent-tabs-mode nil)))
(use-package sly
  :ensure t)


;;; Markdown
(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

(provide 'langs)
;;; langs.el ends here
