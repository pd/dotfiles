(add-hook 'emacs-lisp-mode-hook 'pd/lisp-mode)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode)

(define-key lisp-mode-shared-map (kbd "C-c e") 'eval-print-last-sexp)
