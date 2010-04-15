(autoload 'espresso-mode "espresso" "Major mode for javascript" t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . espresso-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . espresso-mode))

(setq espresso-indent-level 2)
(add-hook 'espresso-mode-hook 'pd/run-coding-hook)

(provide 'pd/js)
