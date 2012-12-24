; ~ in ido moves to $HOME
(defun pd/ido-move-to-home ()
  (interactive)
  (ido-set-current-home)
  (ido-reread-directory))

(defun pd/add-ido-keybindings ()
  (define-key ido-file-dir-completion-map (kbd "~") 'pd/ido-move-to-home))

(defun pd/ido-file-prompt-abbreviate-file-name ()
  (setq dirname (pd/abbreviate-file-name dirname)))

(provide 'pd/ido)
