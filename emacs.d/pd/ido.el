; ~ in ido moves to $HOME
(defun pd/ido-move-to-home ()
  (interactive)
  (ido-set-current-home)
  (ido-reread-directory))

(defun pd/add-ido-keybindings ()
  (define-key ido-file-dir-completion-map (kbd "~") 'pd/ido-move-to-home))

(add-hook 'ido-setup-hook 'pd/add-ido-keybindings)

(defun pd/ido-file-prompt-abbreviate-file-name ()
  (setq dirname (pd/abbreviate-file-name dirname)))

(setq ido-rewrite-file-prompt-functions '(pd/ido-file-prompt-abbreviate-file-name))

(provide 'pd/ido)
