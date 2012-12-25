;; Common to all lisps
(define-minor-mode pd/lisp-mode
  "Easy way to add common hooks / keys to all Lispy modes."
  :lighter "pdl "
  :keymap (ignore))

(add-hook 'pd/lisp-mode-hook 'pd/run-prog-mode-hooks)
(add-hook 'pd/lisp-mode-hook 'paredit-mode)
(add-hook 'pd/lisp-mode-hook 'show-paren-mode)
(add-hook 'pd/lisp-mode-hook 'pd/local-newline-and-indent)

;; Emacs Lisp
(add-hook 'emacs-lisp-mode-hook 'pd/lisp-mode)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

; M-. and M-, for jumping to elisp defuns and back
(when (require 'elisp-slime-nav nil 'noerror)
  (add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode))

(provide 'pd/lisps)
