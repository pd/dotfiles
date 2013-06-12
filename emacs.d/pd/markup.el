(after 'css-mode
  (setq css-indent-offset 2))

(after 'scss-mode
  (setq scss-compile-at-save nil))

(after 'slim-mode
  (add-hook 'slim-mode-hook 'show-paren-mode)
  (add-hook 'slim-mode-hook 'hl-line-mode)
  (add-hook 'slim-mode-hook 'pd/show-trailing-whitespace-mode)
  (add-hook 'slim-mode-hook 'pd/require-final-newline-mode))

(after 'sgml-mode
  (require 'tagedit)
  (defun pd/maybe-tagedit-mode ()
    (unless (string-match-p "\\.erb\\'" (buffer-file-name))
      (tagedit-mode +1)))
  (add-hook 'sgml-mode-hook 'zencoding-mode)
  (add-hook 'html-mode-hook 'pd/maybe-tagedit-mode))

(after 'tagedit
  (tagedit-add-paredit-like-keybindings)
  (tagedit-add-experimental-features)
  (bind-key "s-k" 'windmove-up tagedit-mode-map)
  (bind-key "s-K" 'tagedit-kill-attribute tagedit-mode-map))

(after 'zencoding-mode
  (setq zencoding-insert-flash-time 0.01
        zencoding-indentation 2)

  (after 'sgml-mode
    (bind-key "C-c e" 'zencoding-expand-line sgml-mode-map)))

(provide 'pd/markup)
