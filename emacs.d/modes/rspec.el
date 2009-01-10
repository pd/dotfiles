(add-hook 'rspec-mode-hook
          (lambda ()
            (define-key rspec-mode-map (kbd "C-c s s") 'rspec-run-suite)
            (define-key rspec-mode-map (kbd "C-c s RET") 'rspec-run-examples)
            (define-key rspec-mode-map (kbd "C-c s .") 'rspec-run-example)))
