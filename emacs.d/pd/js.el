(autoload 'espresso-mode "espresso" "Major mode for javascript" t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . espresso-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . espresso-mode))

(defun pd/add-js-bindings ()
  (pd/enable-newline-and-indent espresso-mode-map))

(defun pd/espresso-reindent-less ()
  (define-key espresso-mode-map (kbd ",") 'self-insert-command)
  (define-key espresso-mode-map (kbd ";") 'self-insert-command))

(setq espresso-indent-level 2)
(add-hook 'espresso-mode-hook 'pd/run-coding-hook)
(add-hook 'espresso-mode-hook 'pd/add-js-bindings)
(add-hook 'espresso-mode-hook 'pd/espresso-reindent-less)

(provide 'pd/js)
