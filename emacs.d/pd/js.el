(autoload 'espresso-mode "espresso" "Major mode for javascript" t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . espresso-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . espresso-mode))

(defun pd/add-js-bindings ()
  (pd/enable-newline-and-indent espresso-mode-map))

(setq espresso-indent-level 2)
(add-hook 'espresso-mode-hook 'pd/run-coding-hook)
(add-hook 'espresso-mode-hook 'pd/add-js-bindings)

(provide 'pd/js)
