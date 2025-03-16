;;; welcome.el --- A quick-and-easy dashboard
;;
;;; Commentary:
;; Author: M Cooper Healy
;;
;;; Code:

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


(provide 'welcome)
;;; welcome.el ends here
