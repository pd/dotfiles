(autoload 'rspec-mode "rspec-mode" "Major mode for rspec" t)
(autoload 'rspec-run-suite "rspec-mode" "" t)

(add-to-list 'auto-mode-alist '("_spec.rb\\'" . rspec-mode))

(add-hook 'rspec-mode-hook
          (lambda ()
            (define-key rspec-mode-map (kbd "C-c s s") 'rspec-run-suite)
            (define-key rspec-mode-map (kbd "C-c s RET") 'rspec-run-examples)
            (define-key rspec-mode-map (kbd "C-c s .") 'rspec-run-example)))
