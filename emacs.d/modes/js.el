(autoload 'js2-mode "js2-mode" "Major mode for JS files" t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(eval-after-load 'js2-mode
  '(progn
     (setq js2-electric-keys '())))

(add-hook 'js2-mode-hook
          (lambda ()
            (coding-hook)))
