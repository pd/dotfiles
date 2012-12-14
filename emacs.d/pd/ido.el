; ~ in ido moves to $HOME
(defun pd/ido-move-to-home ()
  (interactive)
  (ido-set-current-home)
  (ido-reread-directory))

(defun pd/add-ido-keybindings ()
  (define-key ido-file-dir-completion-map (kbd "~") 'pd/ido-move-to-home))

(add-hook 'ido-setup-hook 'pd/add-ido-keybindings)
(setq ido-rewrite-file-prompt-rules pd/directory-abbrev-alist)

(provide 'pd/ido)
