(autoload 'haml-mode "haml-mode" "Major mode for HAML files" t)
(autoload 'sass-mode "sass-mode" "Major mode for Sass files" t)
(add-to-list 'auto-mode-alist '("\\.haml\\'" . haml-mode))
(add-to-list 'auto-mode-alist '("\\.sass\\'" . sass-mode))
