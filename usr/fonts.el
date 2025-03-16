;;; fonts.el --- custom font selections
;;
;;; Commentary:
;; Author: M Cooper Healy
;;
;;; Code:

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

(provide 'fonts)
;;; fonts.el ends here