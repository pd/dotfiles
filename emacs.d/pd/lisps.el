;; Common to all lisps
(define-minor-mode pd/lisp-mode
  "Easy way to add common hooks / keys to all Lispy modes."
  :lighter nil)

(pd/define-newline-and-indent lisp-mode-shared-map)

(add-hook 'pd/lisp-mode-hook 'paredit-mode)
(add-hook 'pd/lisp-mode-hook 'show-paren-mode)
(add-hook 'pd/lisp-mode-hook 'subword-mode)

(provide 'pd/lisps)
