(define-project-type emacs-d (generic)
  (look-for "init.el"))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (coding-hook)
            (show-paren-mode t)
            (setq indent-tabs-mode nil)
            (define-key lisp-mode-shared-map (kbd "<return>") 'newline-and-indent)))
