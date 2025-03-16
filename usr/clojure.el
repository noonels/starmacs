;;; packages.el --- my clojure configuration
;;
;;; Commentary:
;; Author: M Cooper Healy
;;
;;; Code:

;; CIDER is considered the best clojure mode for emacs
;; think SLIME for clojure
(use-package cider
  :ensure t)

;; Register Clojure with eglot
(add-hook 'clojure-mode-hook #'eglot-ensure)

(provide 'clojure)
;;; clojure.el ends here
