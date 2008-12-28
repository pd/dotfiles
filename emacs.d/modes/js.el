(autoload 'js2-mode "js2-mode" "Major mode for JS files" t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js2-mode-hook
          (lambda ()
            (coding-hook)
            (define-key js2-mode-map (kbd "<return>") 'newline-and-indent)))
