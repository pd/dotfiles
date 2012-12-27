;; Common to all lisps
(define-minor-mode pd/lisp-mode
  "Easy way to add common hooks / keys to all Lispy modes."
  :lighter nil)

(pd/define-newline-and-indent lisp-mode-shared-map)

(add-hook 'pd/lisp-mode-hook 'paredit-mode)
(add-hook 'pd/lisp-mode-hook 'show-paren-mode)

;; Emacs Lisp
(add-hook 'emacs-lisp-mode-hook 'pd/lisp-mode)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode)

(add-hook 'ielm-mode-hook 'pd/lisp-mode)
(add-hook 'ielm-mode-hook 'elisp-slime-nav-mode)

(provide 'pd/lisps)
