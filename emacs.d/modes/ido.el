(defun pd-ido-move-to-home ()
  (interactive)
  (ido-set-current-home)
  (ido-reread-directory))

(add-hook 'ido-setup-hook
          (lambda ()
            (define-key ido-file-dir-completion-map (kbd "~") 'pd-ido-move-to-home)))
