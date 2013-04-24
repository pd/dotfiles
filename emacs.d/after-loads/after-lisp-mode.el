(add-hook 'emacs-lisp-mode-hook 'pd/lisp-mode)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode)

(define-key lisp-mode-shared-map (kbd "C-c e b")   'eval-buffer)
(define-key lisp-mode-shared-map (kbd "C-c e d")   'eval-defun)
(define-key lisp-mode-shared-map (kbd "C-c e r")   'eval-region)
(define-key lisp-mode-shared-map (kbd "C-c e s")   'eval-last-sexp)
(define-key lisp-mode-shared-map (kbd "C-c e M-s") 'eval-print-last-sexp)
