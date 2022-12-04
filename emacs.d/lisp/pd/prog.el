(require 'f)
(require 'deadgrep)
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

(after 'cue-mode
  (add-hook 'cue-mode-hook 'cue-format-on-save-mode))

(after 'feature-mode
  (add-hook 'feature-mode-hook 'hl-line-mode)
  (add-hook 'feature-mode-hook 'pd/show-trailing-whitespace-mode)
  (add-hook 'feature-mode-hook 'pd/require-final-newline-mode)
  (add-hook 'feature-mode-hook 'pd/electric-indent-incompatible-mode)
  (pd/enable-newline-and-indent feature-mode-map))

(after 'coffee-mode
  (add-hook 'coffee-mode-hook 'pd/electric-indent-incompatible-mode)
  (pd/enable-newline-and-indent feature-mode-map))

(after 'dap-mode
  (setq dap-breakpoints-file (expand-file-name "~/.emacs.d/.crap/dap-breakpoints")))

; some mix of project.el + deadgrep + emacs26 etc causes issues
; with deadgrep until I reload the buffer. no idea why:
; deadgrep--project-root: Invalid function: (roots (project-roots project))
(after 'deadgrep
  (require 'find-func)
  (load (find-library-name "deadgrep")))

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

(after 'jsonnet-mode
  (defun pd/jsonnet-fmt-on-save ()
    (add-hook 'before-save-hook 'jsonnet-reformat-buffer nil t))
  (add-hook 'jsonnet-mode-hook 'pd/jsonnet-fmt-on-save))

(after 'kotlin-mode
  (add-hook 'kotlin-mode-hook #'lsp-deferred))

(after 'lsp
  (require 'helm-lsp)
  (setq lsp-enable-snippet   nil
        lsp-prefer-flymake   nil
        lsp-eldoc-render-all nil
        lsp-ui-doc-enable    nil
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-code-actions-prefix "* ")
  (bind-keys :map lsp-mode-map
             ("s-x" . helm-lsp-code-actions)
             ("s-a" . helm-lsp-workspace-symbol)
             ("s-A" . helm-lsp-global-workspace-symbol)
             ("C-c l r" . lsp-rename)
             ("C-c l R" . lsp-restart-workspace)
             ("C-c l m" . lsp-ui-imenu)
             ("C-c l x" . lsp-find-references)
             ("C-c l i" . lsp-find-implementation)))

(after 'magit
  (setq magit-save-some-buffers nil
        magit-completing-read 'magit-ido-completing-read
        magit-omit-untracked-dir-contents nil
        magit-process-popup-time 10))

(after 'markdown-mode
  (defun pd/markdown-fill-column ()
    (setq fill-column 80))
  (add-hook 'markdown-mode-hook 'pd/markdown-fill-column))

(after 'rego-mode
  (defun pd/rego-mode ()
    (setq tab-width 2))
  (add-hook 'rego-mode-hook 'pd/rego-mode))

(after 'repl-toggle
  (unbind-key "C-c C-z" repl-toggle-mode-map)
  (setq rtog/mode-repl-alist
        '((emacs-lisp-mode . ielm)
          (enh-ruby-mode   . run-ruby)
          (ruby-mode       . run-ruby)
          (elixir-mode     . elixir-mode-iex)
          (js2-mode        . nodejs-repl))))

(after 'rust-mode
  (defun pd/rust-mode ()
    (setq lsp-enable-snippet nil
          lsp-rust-analyzer-completion-add-call-argument-snippets nil))

  (add-hook 'rust-mode-hook #'subword-mode)
  (add-hook 'rust-mode-hook #'lsp-deferred)
  (add-hook 'rust-mode-hook #'flycheck-rust-setup)
  (add-hook 'rust-mode-hook #'rust-enable-format-on-save)
  (add-hook 'rust-mode-hook #'pd/rust-mode)
  (bind-keys :map rust-mode-map
             ("C-^" . lsp-rust-analyzer-join-lines)
             ("s-x" . lsp-execute-code-action)))

(after 'treemacs
  (setq treemacs-persist-file (expand-file-name "~/.emacs.d/.crap/treemacs-persist-file")))

(after 'slim-mode
  (add-hook 'slim-mode-hook 'pd/electric-indent-incompatible-mode)
  (pd/enable-newline-and-indent feature-mode-map))

(after 'terraform-mode
  (add-hook 'terraform-mode-hook #'terraform-format-on-save-mode))

(require 'cue-mode)
(provide 'pd/prog)
