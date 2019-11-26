(require 'f)
(require 'editorconfig)

(editorconfig-mode +1)

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

(after 'hcl-mode
  (defun pd/hclfmt-on-save ()
    "Enable hclfmt on save, if hclfmt is available on `exec-path'."
    (let ((hclfmt (executable-find "hclfmt")))
      (when (and hclfmt (not (eq major-mode 'terraform-mode)))
        (add-hook 'before-save-hook #'pd/hclfmt-buffer nil t))))

  (defun pd/hclfmt-buffer ()
    "Rewrite current buffer with results of hclfmt."
    (interactive)
    (if-let ((hclfmt (executable-find "hclfmt")))
        (let ((buf (get-buffer-create "*hclfmt*")))
          (if (zerop (call-process-region (point-min) (point-max)
                                          "hclfmt" nil buf nil))
              (let ((point (point))
                    (window-start (window-start)))
                (erase-buffer)
                (insert-buffer-substring buf)
                (goto-char point)
                (set-window-start nil window-start))
            (message "hclfmt: %s" (with-current-buffer buf (buffer-string))))
          (kill-buffer buf))))

  (add-hook 'hcl-mode-hook 'pd/hclfmt-on-save))

(after 'kotlin-mode
  (add-hook 'kotlin-mode-hook #'lsp-deferred))

(after 'magit
  (setq magit-save-some-buffers nil
        magit-completing-read 'magit-ido-completing-read
        magit-omit-untracked-dir-contents nil
        magit-process-popup-time 10))

(after 'markdown-mode
  (defun pd/markdown-fill-column ()
    (setq fill-column 80))
  (add-hook 'markdown-mode-hook 'pd/markdown-fill-column))

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
  (add-hook 'rust-mode-hook 'subword-mode)
  (add-hook 'rust-mode-hook 'racer-mode)
  (add-hook 'rust-mode-hook 'rust-enable-format-on-save)
  (add-hook 'racer-mode-hook 'eldoc-mode)

  (defun pd/rust-insert-type-error ()
    (interactive)
    (let ((sym (symbol-at-point)))
      (when sym
        (pd/open-line-after)
        (insert "let () = " (symbol-name sym) ";")
        (beginning-of-line-text)
        (forward-char 4)
        (save-buffer))))

  (bind-key "C-c i t" 'pd/rust-insert-type-error rust-mode-map)

  (setq rust-rustfmt-bin         (f-expand "~/.cargo/bin/rustfmt")
        racer-cmd                (f-expand "~/.cargo/bin/racer")
        racer-rust-src-path      (f-expand "~/vendor/rust/src")))

(after 'slim-mode
  (add-hook 'slim-mode-hook 'pd/electric-indent-incompatible-mode)
  (pd/enable-newline-and-indent feature-mode-map))

(after 'terraform-mode
  (defun pd/maybe-terraform-format-on-save ()
    "Disable terraform-format-on-save-mode for tfvars files."
    (unless (string-suffix-p "tfvars" buffer-file-name)
      (terraform-format-on-save-mode)))

  (add-hook 'terraform-mode-hook 'pd/maybe-terraform-format-on-save))

(provide 'pd/prog)
