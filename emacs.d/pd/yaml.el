(defun pd/add-yaml-keybindings ()
  (define-key yaml-mode-map (kbd "C-c C-p") 'yaml-path/path))

(autoload 'yaml-mode "yaml-mode")
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))

(autoload 'yaml-path/path "yaml-path")

(add-hook 'yaml-mode-hook 'pd/add-yaml-keybindings)

(provide 'pd/yaml)
