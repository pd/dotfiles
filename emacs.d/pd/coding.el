(defun pd/common-prog-mode-keys ()
  (local-set-key (kbd "M-p") 'backward-paragraph)
  (local-set-key (kbd "M-n") 'forward-paragraph)

  ; There ought to be some sort of logic to determine whether these
  ; should be left alone. Not sure yet.
  ;(local-set-key (kbd "<return>") 'newline-and-indent)
  ;(local-set-key (kbd "RET") 'newline-and-indent)
  )

(add-hook 'prog-mode-hook 'pd/common-prog-mode-keys)
(add-hook 'prog-mode-hook 'column-number-mode)
(add-hook 'prog-mode-hook 'show-paren-mode)
(add-hook 'prog-mode-hook 'hl-line-mode)
(add-hook 'prog-mode-hook 'pd/turn-on-hl-watchwords)

(defun pd/run-prog-mode-hooks ()
  (run-hooks 'prog-mode-hook))

(provide 'pd/coding)
