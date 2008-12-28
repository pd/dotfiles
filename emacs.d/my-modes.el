(require 'my-defuns)
(require 'my-jumps)

(autoload 'ruby-mode "ruby-mode" "Major mode for ruby" t)
(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile\\'" . ruby-mode))
(add-hook 'ruby-mode-hook
          (lambda ()
            (require 'ruby-electric)
            (coding-hook)
            (define-key ruby-mode-map (kbd "C-c j s")  'jump-to-spec-file)
            (define-key ruby-mode-map (kbd "C-c j i")  'jump-to-implementation-file)
            (define-key ruby-mode-map (kbd "<return>") 'newline-and-indent)))

(autoload 'haml-mode "haml-mode" "Major mode for HAML files" t)
(autoload 'sass-mode "sass-mode" "Major mode for Sass files" t)
(add-to-list 'auto-mode-alist '("\\.haml\\'" . haml-mode))
(add-to-list 'auto-mode-alist '("\\.sass\\'" . sass-mode))

(autoload 'js2-mode "js2-mode" "Major mode for JS files" t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js2-mode-hook
          (lambda ()
            (coding-hook)
            (define-key js2-mode-map (kbd "<return>") 'newline-and-indent)))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (coding-hook)
            (define-key lisp-mode-shared-map (kbd "<return>") 'newline-and-indent)))

(autoload 'yaml-mode "yaml-mode" "Major mode for YAML files" t)
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-mode))

(autoload 'git-blame-mode "git-blame" "Minor mode for incremental blame for Git" t)

(provide 'my-modes)
