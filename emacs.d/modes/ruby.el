(autoload 'ruby-mode "ruby-mode" "Major mode for ruby" t)

(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile\\'" . ruby-mode))

(add-hook 'ruby-mode-hook
          (lambda ()
            (require 'ruby-electric)
            (coding-hook)
            (setq indent-tabs-mode nil)
            (define-key ruby-mode-map (kbd "C-c j s")  'jump-to-spec-file)
            (define-key ruby-mode-map (kbd "C-c j i")  'jump-to-implementation-file)
            (define-key ruby-mode-map (kbd "<return>") 'newline-and-indent)))
