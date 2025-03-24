;;; init.el --- Emacs configuration
;;; Commentary:
;;; Code:
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;;; Package Management
;; We assume that we will want packages from MELPA, the most popular emacs package registry, as well as ELPA, the official GNU repository, and MELPA Stable (which is exactly what it sounds like).
(require 'package)
(setq package-archives
      '(("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("GNU ELPA"     . "https://elpa.gnu.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("GNU ELPA"     . 10)
        ("MELPA"        . 0)))

(add-to-list 'load-path "~/.emacs.d/modules") ; add local files

(setq use-package-vc-prefer-newest t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e"
     "000ae191922c662e7f89eae84932415f9d7f5c3045b167b3375c2ad9b62a0c78"
     "4f7b78f1db71645999a8d632fe1d5cf863750a75b2153d429e59051827e439f2"
     "f40d0dc5fd64fef08959e2f5a35050baeb98faef572c233b7dcc3f89f0feed69"
     "574167ab321bb3041545e414a466cb30c48ec41d4ec27593c58be78d837575cc"
     "02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644"
     "0e83cec64ea5e9d63769fd8644936d367f624f83d7cd5310c949f74b8975d305"
     default))
 '(org-agenda-files nil)
 '(org-modern-block-fringe t)
 '(package-selected-packages
   '(ace-window add-node-modules-path anisochromatic-theme autothemer
                base-line cider company company-box doom-modeline
                doom-themes flycheck forge git-gutter go-mode helpful
                hl-todo magit marginalia markdown-mode meow mmm-mode
                orderless org-roam rainbow-delimiters sly sqlite3
                transient treemacs vertico vterm web-mode why-this
                with-editor zig-mode))
 '(package-vc-selected-packages
   '((base-line :url "http://github.com/isomatter-labs/base-line")
     (anisochromatic-theme :url
                           "http://github.com/isomatter-labs/anisochromatic-emacs")))
 '(warning-suppress-types
   '(((defvaralias losing-value org-tab-first-hook)) (use-package)
     (use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blamer-face ((t :foreground "#7a88cf" :background nil :height 140 :italic t)))
 '(treemacs-fringe-indicator-face ((t (:inherit font-lock-doc-face))))
 '(treemacs-git-ignored-face ((t (:inherit (shadow)))))
 '(whitespace-newline ((t (:foreground "#525252"))))
 '(whitespace-tab ((t (:foreground "#525252")))))
(put 'downcase-region 'disabled nil)

;; CIDER is considered the best clojure mode for emacs
;; think SLIME for clojure
(use-package cider
  :ensure t)

;; Register Clojure with eglot
(add-hook 'clojure-mode-hook #'eglot-ensure)

;; Company-mode
  (use-package company
    :ensure t
    :hook ((prog-mode) . (lambda () (company-mode)))
    :bind (:map company-mode-map
                ("<tab>" . 'company-indent-or-complete-common)
                :map company-active-map
                ("C-n" . 'company-select-next-or-abort)
                ("C-p" . 'company-select-previous-or-abort))
    :custom
    (company-idle-delay nil) ; don't try to complete until asked
    (company-minimum-prefix-length 1)
    (company-tooltip-align-annotations t)
    (lsp-completion-provider :capf) ; used for eglot integration

    (company-show-quick-access t)
    :config
    (company-tng-configure-default))

  (use-package company-box
    :ensure t
    :hook (company-mode . company-box-mode))


;; Tab to complete suggestions (other completion methods have their /own/ keybindings)
(setq tab-always-indent 'complete)

;; Copilot
  (defun starmacs/copilot-mode-hook ()
    (when (and (boundp '*starmacs/copilot-enabled*)
               ,*starmacs/copilot-enabled*)
      copilot-mode-hook))

  (use-package copilot
    :vc (:url "http://github.com/zerolfx/copilot.el" :files ("dist" "*.el"))
    :hook (prog-mode . copilot-mode)
    :config
    (define-key copilot-completion-map (kbd "C-f") 'copilot-accept-completion) ; using forward motion to accept completion like Warp
    (define-key copilot-completion-map (kbd "<right>") 'copilot-accept-completion)
    (unless (copilot-installed-version)
  	  (copilot-install-server))
    :ensure t)

;; Chat-GPT
  (setq starmacs/chatgpt-api-key (expand-file-name "chatgpt-api-key.txt"))

  (use-package chatgpt-shell
    :ensure t
    :vc (:url "http://github.com/xenodium/chatgpt-shell" :files ("dist" "*.el"))
    :config
    (unless
      (file-exists-p starmacs/chatgpt-api-key)
    (make-empty-file starmacs/chatgpt-api-key))

    (setq chatgpt-shell-openai-key (replace-regexp-in-string "\n\\'" "" (with-temp-buffer
                                      (insert-file-contents (expand-file-name "chatgpt-api-key.txt"))
                                      (buffer-string)))))



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


(defvar starmacs/fixed-pitch-height 150)
(defvar starmacs/variable-pitch-height 130)
(setq-default line-spacing 0.2)

(defvar starmacs/variable-pitch-font "Mona Sans")
(defvar starmacs/title-font "Hubot-Sans")
(defvar starmacs/fixed-pitch-font "Berkeley Mono")


(set-face-attribute 'default nil :font starmacs/fixed-pitch-font :height starmacs/fixed-pitch-height)
(set-face-attribute 'fixed-pitch nil :font starmacs/fixed-pitch-font :height starmacs/fixed-pitch-height)

(set-face-attribute 'variable-pitch nil :font starmacs/variable-pitch-font :height starmacs/variable-pitch-height)
(set-face-attribute 'mode-line nil
                    :font starmacs/fixed-pitch-font)


(setq starmacs/fixed-pitch-font "Berkeley Mono")
(set-face-attribute 'fixed-pitch nil :font starmacs/fixed-pitch-font :height starmacs/fixed-pitch-height)
(set-face-attribute 'mode-line nil :font starmacs/fixed-pitch-font :height (+ starmacs/fixed-pitch-height 15)) ; add a little extra height to the mode line

  (use-package ligature
    :config
    ;; Enable the "www" ligature in every possible major mode
    (ligature-set-ligatures 't '("www"))
    ;; Enable traditional ligature support in eww-mode, if the
    ;; `variable-pitch' face supports it
    (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
    (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                         ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                         "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                         "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                         "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                         "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                         "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                         "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                         ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                         "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                         "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                         "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                         "\\\\" "://" "<-"))
    ;; Enables ligature checks globally in all buffers. You can also do it
    ;; per mode with `ligature-mode'.
    (global-ligature-mode t))


  (defun starmacs/revert-buffer-no-confirm ()
    "Revert the current buffer without asking permission"
    (interactive)
    (revert-buffer :ignore-auto :noconfirm))

  (global-set-key (kbd "<f5>") 'starmacs/revert-buffer-no-confirm)
  (global-set-key (kbd "s-r") 'starmacs/revert-buffer-no-confirm)

;;   (defun meow-setup ()
;;     (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
;;     (meow-motion-overwrite-define-key
;;      '("j" . meow-next)
;;      '("k" . meow-prev)
;;      '("<escape>" . ignore))
;;     (meow-leader-define-key
;;      ;; SPC j/k will run the original command in MOTION state.
;;      '("j" . "H-j")
;;      '("k" . "H-k")
;;      ;; Use SPC (0-9) for digit arguments.
;;      '("1" . meow-digit-argument)
;;      '("2" . meow-digit-argument)
;;      '("3" . meow-digit-argument)
;;      '("4" . meow-digit-argument)
;;      '("5" . meow-digit-argument)
;;      '("6" . meow-digit-argument)
;;      '("7" . meow-digit-argument)
;;      '("8" . meow-digit-argument)
;;      '("9" . meow-digit-argument)
;;      '("0" . meow-digit-argument)
;;      '("/" . meow-keypad-describe-key)
;;      '("?" . meow-cheatsheet))
;;     (meow-normal-define-key
;;      '("0" . meow-expand-0)
;;      '("9" . meow-expand-9)
;;      '("8" . meow-expand-8)
;;      '("7" . meow-expand-7)
;;      '("6" . meow-expand-6)
;;      '("5" . meow-expand-5)
;;      '("4" . meow-expand-4)
;;      '("3" . meow-expand-3)
;;      '("2" . meow-expand-2)
;;      '("1" . meow-expand-1)
;;      '("-" . negative-argument)
;;      '(";" . meow-reverse)
;;      '("," . meow-inner-of-thing)
;;      '("." . meow-bounds-of-thing)
;;      '("[" . meow-beginning-of-thing)
;;      '("]" . meow-end-of-thing)
;;      '("a" . meow-append)
;;      '("A" . meow-open-below)
;;      '("b" . meow-back-word)
;;      '("B" . meow-back-symbol)
;;      '("c" . meow-change)
;;      '("d" . meow-delete)
;;      '("D" . meow-backward-delete)
;;      '("e" . meow-next-word)
;;      '("E" . meow-next-symbol)
;;      '("f" . meow-find)
;;      '("g" . meow-cancel-selection)
;;      '("G" . meow-grab)
;;      '("h" . meow-left)
;;      '("H" . meow-left-expand)
;;      '("i" . meow-insert)
;;      '("I" . meow-open-above)
;;      '("j" . meow-next)
;;      '("J" . meow-next-expand)
;;      '("k" . meow-prev)
;;      '("K" . meow-prev-expand)
;;      '("l" . meow-right)
;;      '("L" . meow-right-expand)
;;      '("m" . meow-join)
;;      '("n" . meow-search)
;;      '("o" . meow-block)
;;      '("O" . meow-to-block)
;;      '("p" . meow-yank)
;;      '("q" . meow-quit)
;;      '("Q" . meow-goto-line)
;;      '("r" . meow-replace)
;;      '("R" . meow-swap-grab)
;;      '("s" . meow-kill)
;;      '("t" . meow-till)
;;      '("u" . meow-undo)
;;      '("U" . meow-undo-in-selection)
;;      '("v" . meow-visit)
;;      '("w" . meow-mark-word)
;;      '("W" . meow-mark-symbol)
;;      '("x" . meow-line)
;;      '("X" . meow-goto-line)
;;      '("y" . meow-save)
;;      '("Y" . meow-sync-grab)
;;      '("z" . meow-pop-selection)
;;      '("'" . repeat)
;;      '("<escape>" . ignore)))

;; (use-package meow
;;     :ensure t
;;     :config
;;       (meow-setup)
;;       (meow-global-mode))

;; Use easymotion-like bindings for window hopping
  (use-package ace-window
    :ensure t
    :config
    (global-set-key (kbd "C-x o") 'ace-window))

;; Display prompts for keybindings
(use-package which-key
    :ensure t
    :init (which-key-mode)
    :diminish which-key-mode
    :config
    (setq which-key-idle-delay 1))


(use-package python-black
  :demand t
  :after python
  :hook (python-ts-mode . python-black-on-save-mode-enable-dwim))

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

(use-package protobuf-mode
  :straight t)

(use-package gdscript-mode
  :straight (gdscript-mode
             :type git
             :host github
             :repo "godotengine/emacs-gdscript-mode"))

; (add-to-list 'eglot-server-programs '(f90-mode . ("fortls" "--notify_init" "--nthreads=4")))

;; no tabs in lisp
(add-hook 'emacs-lisp-mode-hook (lambda () (setq-local indent-tabs-mode nil)))
(add-hook 'lisp-mode-hook (lambda () (setq-local indent-tabs-mode nil)))
(use-package sly
  :ensure t)


(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))


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


  (use-package project
    :ensure t)

      (use-package vertico
    :ensure t
    :bind (:map vertico-map
                ("C-j" . vertico-next)
                ("C-k" . vertico-previous)
                ("C-f" . vertico-exit)
                :map minibuffer-local-map
                ("M-h" . backward-kill-word))
    :custom
    (vertico-cycle t)
    :init
    (vertico-mode))

  (use-package savehist
    :init
    (savehist-mode))

  (use-package marginalia
    :after vertico
    :ensure t
    :custom
    (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
    :init
    (marginalia-mode))

      ;; Example configuration for Consult
  (use-package consult
    ;; Replace bindings. Lazily loaded due by `use-package'.
    :bind (;; C-c bindings (mode-specific-map)
           ("C-s" . consult-line)
           ("C-c h" . consult-history)
           ("C-c m" . consult-mode-command)
           ("C-c k" . consult-kmacro)
           ;; C-x bindings (ctl-x-map)
           ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
           ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
           ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
           ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
           ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
           ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
           ;; Custom M-# bindings for fast register access
           ("M-#" . consult-register-load)
           ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
           ("C-M-#" . consult-register)
           ;; Other custom bindings
           ("M-y" . consult-yank-pop)                ;; orig. yank-pop
           ;; M-g bindings (goto-map)
           ("M-g e" . consult-compile-error)
           ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
           ("M-g g" . consult-goto-line)             ;; orig. goto-line
           ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
           ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
           ("M-g m" . consult-mark)
           ("M-g k" . consult-global-mark)
           ("M-g i" . consult-imenu)
           ("M-g I" . consult-imenu-multi)
           ;; M-s bindings (search-map)
           ("M-s d" . consult-find)
           ("M-s D" . consult-locate)
           ("M-s g" . consult-grep)
           ("M-s G" . consult-git-grep)
           ("M-s r" . consult-ripgrep)
           ("M-s l" . consult-line)
           ("M-s L" . consult-line-multi)
           ("M-s m" . consult-multi-occur)
           ("M-s k" . consult-keep-lines)
           ("M-s u" . consult-focus-lines)
           ;; Isearch integration
           ("M-s e" . consult-isearch-history)
           :map isearch-mode-map
           ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
           ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
           ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
           ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
           ;; Minibuffer history
           :map minibuffer-local-map
           ("M-s" . consult-history)                 ;; orig. next-matching-history-element
           ("M-r" . consult-history))                ;; orig. previous-matching-history-element

    ;; Enable automatic preview at point in the *Completions* buffer. This is
    ;; relevant when you use the default completion UI.
    :hook (completion-list-mode . consult-preview-at-point-mode)

    ;; The :init configuration is always executed (Not lazy)
    :init

    ;; Optionally configure the register formatting. This improves the register
    ;; preview for `consult-register', `consult-register-load',
    ;; `consult-register-store' and the Emacs built-ins.
    (setq register-preview-delay 0.5
          register-preview-function #'consult-register-format)

    ;; Optionally tweak the register preview window.
    ;; This adds thin lines, sorting and hides the mode line of the window.
    (advice-add #'register-preview :override #'consult-register-window)

    ;; Use Consult to select xref locations with preview
    (setq xref-show-xrefs-function #'consult-xref
          xref-show-definitions-function #'consult-xref)

    ;; Configure other variables and modes in the :config section,
    ;; after lazily loading the package.
    :config

    ;; Optionally configure preview. The default value
    ;; is 'any, such that any key triggers the preview.
    ;; (setq consult-preview-key 'any)
    ;; (setq consult-preview-key (kbd "M-."))
    ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
    ;; For some commands and buffer sources it is useful to configure the
    ;; :preview-key on a per-command basis using the `consult-customize' macro.
    (consult-customize
     consult-theme
     :preview-key '(:debounce 0.2 any)
     consult-ripgrep consult-git-grep consult-grep
     consult-bookmark consult-recent-file consult-xref
     consult--source-bookmark consult--source-recent-file
     consult--source-project-recent-file)

    ;; Optionally configure the narrowing key.
    ;; Both < and C-+ work reasonably well.
    (setq consult-narrow-key "<") ;; (kbd "C-+")

    ;; Optionally make narrowing help available in the minibuffer.
    ;; You may want to use `embark-prefix-help-command' or which-key instead.
    ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

    ;; By default `consult-project-function' uses `project-root' from project.el.
    ;; Optionally configure a different project root function.
    ;; There are multiple reasonable alternatives to chose from.
        ;; (setq consult-project-function #'consult--default-project--function)
        ;; (autoload 'projectile-project-root "projectile")
    ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
        ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
        ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
    )

(use-package orderless
    :ensure t
    :custom
    (completion-styles '(orderless basic))
    (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package helpful
    :ensure t
    :bind
    ([remap describe-function] . helpful-function)
    ([remap describe-command]  . helpful-command)
    ([remap describe-variable] . helpful-variable)
    ([remap describe-key]      . helpful-key))


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

  (use-package magit
    :ensure t)

  ;; forge allows magit to connect to Github
  (use-package forge
    :ensure t
    :after magit)

(use-package git-gutter
    :ensure t
    :config
    (global-git-gutter-mode 't))

(use-package why-this
    :ensure t
    :custom (why-this-idle-delay 0)
    :bind
    ("C-c b" . why-this-mode)
    ("C-c w" . why-this))


(defun starmacs/no-linum ()
  "Turn off line-numbers-mode."
     (display-line-numbers-mode -1))

  (defun starmacs/welcome ()
     "Show minimal *welcome* buffer"
    (interactive)
     (delete-other-windows)
     (with-current-buffer (get-buffer-create "*Welcome*")
       (setq truncate-lines t)
       (starmacs/no-linum)
       (let* ((buffer-read-only)
              (image-path "~/.emacs.d/img/emacs.png")
              (image (create-image image-path))
              (size (image-size image))
              (height (cdr size))
              (width (car size))
              (top-margin (floor (/ (- (window-height) height 3) 2)))
              (left-margin (floor (/ (- (window-width) width) 2)))
              (title "A hackable text editor for the 21st Century!"))
         (erase-buffer)
         (setq mode-line-format nil)
         (goto-char (point-min))
         (insert (make-string top-margin ?\n ))
         (insert (make-string left-margin ?\ ))
         (insert-image image)
         (insert "\n\n\n")
         (insert (make-string (floor (/ (- (window-width) (string-width title)) 2)) ?\ ))
         (insert title))
       (setq cursor-type nil)
       (read-only-mode +1)
       (switch-to-buffer (current-buffer))
       (local-set-key (kbd "q") 'kill-this-buffer)))

  (when (< (length command-line-args) 2)
    (add-hook 'emacs-startup-hook (lambda ()
                                   (when (display-graphic-p)
                                     (starmacs/welcome)))))


