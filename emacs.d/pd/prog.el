(require 'f)

(define-minor-mode pd/show-trailing-whitespace-mode
  "Enables `show-trailing-whitespace'."
  :lighter nil
  (progn (setq show-trailing-whitespace pd/show-trailing-whitespace-mode)))

(define-minor-mode pd/require-final-newline-mode
  "Enables `require-final-newline'."
  :lighter nil
  (progn (setq require-final-newline pd/require-final-newline-mode)))

(define-minor-mode pd/electric-indent-incompatible-mode
  "Disables `electric-indent-mode' in a buffer-local fashion."
  :lighter nil
  (progn
    (if pd/electric-indent-incompatible-mode
        (progn
          (set (make-local-variable 'electric-indent-functions)
               (list (lambda (arg) 'no-indent))))
      (set (make-local-variable 'electric-indent-functions) nil))))

(add-hook 'prog-mode-hook 'show-paren-mode)
(add-hook 'prog-mode-hook 'hl-line-mode)
(add-hook 'prog-mode-hook 'pd/show-trailing-whitespace-mode)
(add-hook 'prog-mode-hook 'pd/require-final-newline-mode)
(add-hook 'prog-mode-hook 'turn-on-wrap-region-mode)

(after 'feature-mode
  (add-hook 'feature-mode-hook 'hl-line-mode)
  (add-hook 'feature-mode-hook 'pd/show-trailing-whitespace-mode)
  (add-hook 'feature-mode-hook 'pd/require-final-newline-mode)
  (add-hook 'feature-mode-hook 'pd/electric-indent-incompatible-mode)
  (pd/enable-newline-and-indent feature-mode-map))

(after 'coffee-mode
  (add-hook 'coffee-mode-hook 'pd/electric-indent-incompatible-mode)
  (pd/enable-newline-and-indent feature-mode-map))

(after 'elixir-mode
  (add-hook 'elixir-mode-hook 'pd/electric-indent-incompatible-mode))

(after 'slim-mode
  (add-hook 'slim-mode-hook 'pd/electric-indent-incompatible-mode)
  (pd/enable-newline-and-indent feature-mode-map))

(after 'magit
  (setq magit-save-some-buffers nil
        magit-completing-read 'magit-ido-completing-read
        magit-omit-untracked-dir-contents nil
        magit-process-popup-time 10))

(after 'repl-toggle
  (unbind-key "C-c C-z" repl-toggle-mode-map)
  (setq rtog/mode-repl-alist
        '((emacs-lisp-mode . ielm)
          (enh-ruby-mode   . run-ruby)
          (ruby-mode       . run-ruby)
          (elixir-mode     . elixir-mode-iex)
          (js2-mode        . nodejs-repl))))

(after 'rust-mode
  (require 'racer)
  (require 'company-racer)
  (add-hook 'rust-mode-hook 'subword-mode)
  (add-hook 'rust-mode-hook 'racer-mode)
  (setq racer-cmd (f-expand "~/vendor/racer/target/release/racer")
        company-racer-executable (f-expand "~/vendor/racer/target/release/racer"))
  (add-to-list 'company-backends 'company-racer))

(provide 'pd/prog)
