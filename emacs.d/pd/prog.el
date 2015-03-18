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
  (bind-key "RET" 'newline-and-indent feature-mode-map))

(after 'coffee-mode
  (add-hook 'coffee-mode-hook 'pd/electric-indent-incompatible-mode)
  (bind-key "RET" 'newline-and-indent coffee-mode-map))

(after 'go-mode
  (require 'company-go)
  (add-hook 'go-mode-hook 'go-eldoc-setup)
  (add-hook 'go-mode-hook
            (lambda () (setq tab-width 4)))
  (add-hook 'before-save-hook 'gofmt-before-save)

  (bind-key "M-." 'godef-jump)
  (bind-key "C-c t ." 'go-test-current-test go-mode-map)
  (bind-key "C-c t f" 'go-test-current-file go-mode-map)
  (bind-key "C-c t p" 'go-test-current-project go-mode-map)

  ;; dunno why, but godoc refuses to function despite being in my path:
  ;;    godoc: zsh:1: command not found: godoc
  ;; sadly, go-mode does not expose an option to change the command,
  ;; so i'll just hack that in myself.
  (defun godoc (query)
    "Show Go documentation for QUERY, much like M-x man."
    (interactive (list (godoc--read-query)))
    (unless (string= query "")
      (set-process-sentinel
       (start-process-shell-command "godoc" (godoc--get-buffer query)
                                    (concat (executable-find "godoc") " " query))
       'godoc--buffer-sentinel)
      nil))

  ;; ugh why is this not in melpa or such
  (let* ((gopath (or (getenv "GOPATH") "/does-not-exist/most-likely"))
         (oracle-el (f-join gopath "src/golang.org/x/tools/cmd/oracle/oracle.el")))
    (when (and (f-exists? oracle-el)
               (executable-find "oracle"))
      (add-to-list 'load-path (f-parent oracle-el))
      (require 'go-oracle oracle-el)
      (setq go-oracle-command (executable-find "oracle"))
      (add-hook 'go-mode-hook 'go-oracle-mode))))

(after 'elixir-mode
  (add-hook 'elixir-mode-hook 'pd/electric-indent-incompatible-mode))

(after 'slim-mode
  (add-hook 'slim-mode-hook 'pd/electric-indent-incompatible-mode)
  (bind-key "RET" 'newline-and-indent slim-mode-map))

(after 'magit
  (pd/load-ext 'magit)
  (bind-key "C-x m S" 'pd/magit-insert-submodule-summary git-commit-mode-map)
  (bind-key "C-x m s" 'pd/magit-insert-signoff git-commit-mode-map)
  (bind-key "C-x m a" 'pd/magit-insert-author  git-commit-mode-map)
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
  (add-hook 'rust-mode-hook 'subword-mode)

  (ignore (let ((racer-dir (f-expand "~/vendor/racer/editors")))
            (when (and (f-file? (f-join racer-dir "racer.el"))
                       (f-executable? (f-join racer-dir "target/release/racer")))
              (add-to-list 'load-path racer-dir)
              (require 'racer)
              (setq racer-cmd (f-join racer-dir "target/release/racer"))))))

(provide 'pd/prog)
