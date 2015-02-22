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

  (bind-key "C-c e b"   'eval-buffer          lisp-mode-shared-map)
  (bind-key "C-c e d"   'eval-defun           lisp-mode-shared-map)
  (bind-key "C-c e r"   'eval-region          lisp-mode-shared-map)
  (bind-key "C-c e s"   'eval-last-sexp       lisp-mode-shared-map)
  (bind-key "C-c e M-s" 'eval-print-last-sexp lisp-mode-shared-map))

(after 'ielm
  (add-hook 'ielm-mode-hook 'pd/lisp-mode)
  (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'ielm-mode-hook 'elisp-slime-nav-mode))

(after 'slime
  (setq slime-protocol-version 'ignore
        slime-net-coding-system 'utf-8-unix
        slime-complete-symbol-function 'slime-fuzzy-complete-symbol)

  ;; (add-hook 'slime-mode-hook 'set-up-slime-ac)
  ;; (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)

  ;; (after 'auto-complete
  ;;   (add-to-list 'ac-modes 'slime-repl-mode))
  )

(after 'cider
  (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
  (add-hook 'cider-repl-mode-hook 'pd/lisp-mode)
  ;; (add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
  ;; (add-hook 'cider-mode-hook 'ac-nrepl-setup)
  (setq nrepl-hide-special-buffers t)

  ;; (after 'auto-complete
  ;;   (add-to-list 'ac-modes 'cider-repl-mode))
  )

(after 'clojure-mode
  (add-hook 'clojure-mode-hook 'pd/lisp-mode))

(provide 'pd/lisp)
