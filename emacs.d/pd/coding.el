(pd/define-newline-and-indent prog-mode-map)

(add-hook 'prog-mode-hook 'show-paren-mode)
(add-hook 'prog-mode-hook 'hl-line-mode)
(add-hook 'prog-mode-hook 'pd/turn-on-hl-watchwords)

(defun pd/run-prog-mode-hooks ()
  (run-hooks 'prog-mode-hook))

(provide 'pd/coding)
