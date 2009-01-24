(autoload 'ruby-mode "ruby-mode" "Major mode for ruby" t)
(autoload 'run-ruby "inf-ruby" "Inferior mode for ruby" t)

(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile\\'" . ruby-mode))

(add-hook 'ruby-mode-hook
          (lambda ()
            (require 'ruby-electric)
            (coding-hook)
            (define-key ruby-mode-map (kbd "C-c j s")  'rspec-jump-to-spec-file)
            (define-key ruby-mode-map (kbd "C-c j i")  'rspec-jump-to-implementation-file)
            (define-key ruby-mode-map (kbd "<return>") 'newline-and-indent)))
