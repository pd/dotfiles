(autoload 'magit-status "magit" "Major mode for git interaction" t)
(global-set-key (kbd "C-M-g") 'magit-status)

(setq magit-save-some-buffers nil
      magit-completing-read 'ido-completing-read
      magit-omit-untracked-dir-contents t) ; show unstaged dir, not its contents

(defun pd/magit-insert-submodule-summary ()
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (insert "\n" (magit-git-string "submodule" "summary"))))

(defun pd/add-magit-commit-keybindings ()
  (define-key magit-log-edit-mode-map (kbd "C-M-s")
    'pd/magit-insert-submodule-summary))

(add-hook 'magit-mode-hook 'pd/turn-off-show-trailing-whitespace)
(add-hook 'magit-log-edit-mode-hook 'pd/add-magit-commit-keybindings)

(provide 'pd/magit)
