(defun pd/magit-setup ()
  (setq magit-save-some-buffers nil
        magit-completing-read 'ido-completing-read
        magit-omit-untracked-dir-contents t) ; show unstaged dir, but not its contents
  (add-hook 'magit-mode-hook 'pd/turn-off-show-trailing-whitespace)
  (add-hook 'magit-log-edit-mode-hook 'pd/add-magit-commit-keybindings))

(defun pd/magit-insert-submodule-summary ()
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (insert "\n" (magit-git-string "submodule" "summary"))))

(defun pd/add-magit-commit-keybindings ()
  (define-key magit-log-edit-mode-map (kbd "C-M-s") 'pd/magit-insert-submodule-summary))

(provide 'pd/magit)
