(add-to-list 'auto-mode-alist '("\\.json\\'" . js-mode))

(defun pd/add-js-bindings ()
  (pd/enable-newline-and-indent js-mode-map))

(setq js-indent-level 2
      js-auto-indent-flag nil)

(add-hook 'js-mode-hook 'pd/run-coding-hook)
(add-hook 'js-mode-hook 'pd/add-js-bindings)

(defun pd/enable-moz-minor-mode ()
  (moz-minor-mode 1))

(autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)
(add-hook 'js-mode-hook 'pd/enable-moz-minor-mode)

(provide 'pd/js)
