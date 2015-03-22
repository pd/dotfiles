(define-minor-mode pd/lisp-mode
  "Easy way to add common hooks / keys to all Lispy modes."
  :lighter nil)

(add-hook 'pd/lisp-mode-hook 'paredit-mode)
(add-hook 'pd/lisp-mode-hook 'show-paren-mode)
(add-hook 'pd/lisp-mode-hook 'subword-mode)

(after 'lisp-mode
  (add-hook 'emacs-lisp-mode-hook 'pd/lisp-mode)
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode)

  (bind-keys :map lisp-mode-shared-map
             ("C-c e b"   . eval-buffer)
             ("C-c e d"   . eval-defun)
             ("C-c e r"   . eval-region)
             ("C-c e s"   . eval-last-sexp)
             ("C-c e M-s" . eval-print-last-sexp)))

(after 'ielm
  (add-hook 'ielm-mode-hook 'pd/lisp-mode)
  (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'ielm-mode-hook 'elisp-slime-nav-mode))

(after 'slime
  (setq slime-protocol-version 'ignore
        slime-net-coding-system 'utf-8-unix
        slime-complete-symbol-function 'slime-fuzzy-complete-symbol))

(after 'cider
  (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
  (add-hook 'cider-repl-mode-hook 'pd/lisp-mode)
  (setq nrepl-hide-special-buffers t))

(after 'clojure-mode
  (add-hook 'clojure-mode-hook 'pd/lisp-mode))

(provide 'pd/lisp)
