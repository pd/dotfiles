(define-minor-mode pd/show-trailing-whitespace-mode
  "Enables `show-trailing-whitespace'."
  :lighter nil
  (progn (setq show-trailing-whitespace pd/show-trailing-whitespace-mode)))

(define-minor-mode pd/require-final-newline-mode
  "Enables `require-final-newline'."
  :lighter nil
  (progn (setq require-final-newline pd/require-final-newline-mode)))

(define-minor-mode pd/electric-indent-incompatible-mode
  "Disables `electric-indent-mode' in a buffer-local fashion."
  :lighter nil
  (progn
    (if pd/electric-indent-incompatible-mode
        (progn
          (set (make-local-variable 'electric-indent-functions)
               (list (lambda (arg) 'no-indent))))
      (set (make-local-variable 'electric-indent-functions) nil))))

(add-hook 'prog-mode-hook 'show-paren-mode)
(add-hook 'prog-mode-hook 'hl-line-mode)
(add-hook 'prog-mode-hook 'fic-ext-mode)
(add-hook 'prog-mode-hook 'pd/show-trailing-whitespace-mode)
(add-hook 'prog-mode-hook 'pd/require-final-newline-mode)

(after 'feature-mode
  (add-hook 'feature-mode-hook 'pd/electric-indent-incompatible-mode)
  (bind-key "RET" 'newline-and-indent feature-mode-map))

(after 'coffee-mode
  (add-hook 'coffee-mode-hook 'pd/electric-indent-incompatible-mode)
  (bind-key "RET" 'newline-and-indent coffee-mode-map))

(after 'slim-mode
  (add-hook 'slim-mode-hook 'pd/electric-indent-incompatible-mode)
  (bind-key "RET" 'newline-and-indent slim-mode-map))

(after 'magit
  (pd/load-ext 'magit)
  (bind-key "C-x m S" 'pd/magit-insert-submodule-summary magit-log-edit-mode-map)
  (bind-key "C-x m s" 'pd/magit-insert-signoff magit-log-edit-mode-map)
  (bind-key "C-x m a" 'pd/magit-insert-author  magit-log-edit-mode-map)
  (setq magit-save-some-buffers nil
        magit-completing-read 'magit-ido-completing-read
        magit-omit-untracked-dir-contents nil
        magit-process-popup-time 10))

(provide 'pd/prog)
