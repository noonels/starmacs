;;; hot-reload.el --- keeping files live
;;
;;; Commentary:
;; Author: M Cooper Healy
;;
;;; Code:

  (defun starmacs/revert-buffer-no-confirm ()
    "Revert the current buffer without asking permission"
    (interactive)
    (revert-buffer :ignore-auto :noconfirm))

  (global-set-key (kbd "<f5>") 'starmacs/revert-buffer-no-confirm)
  (global-set-key (kbd "s-r") 'starmacs/revert-buffer-no-confirm)

(provide 'keymap)
;;; hot-reload.el ends here