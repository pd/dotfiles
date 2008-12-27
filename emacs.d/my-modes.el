(autoload 'ruby-mode "ruby-mode" "Major mode for ruby" t)
(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile\\'" . ruby-mode))
(add-hook 'ruby-mode-hook
          '(lambda ()
             (require 'ruby-electric)))

(autoload 'haml-mode "haml-mode" "Major mode for HAML files" t)
(autoload 'sass-mode "sass-mode" "Major mode for Sass files" t)
(add-to-list 'auto-mode-alist '("\\.haml\\'" . haml-mode))
(add-to-list 'auto-mode-alist '("\\.sass\\'" . sass-mode))

(autoload 'js2-mode "js2-mode" "Major mode for JS files" t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(autoload 'yaml-mode "yaml-mode" "Major mode for YAML files" t)
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-mode))

(autoload 'git-blame-mode "git-blame" "Minor mode for incremental blame for Git" t)

(provide 'my-modes)