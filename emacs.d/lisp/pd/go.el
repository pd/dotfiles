(require 'f)
(require 'dash)

(defun pd/configure-go-mode ()
  (setq tab-width 4)
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(after 'go-mode
  (require 'lsp)

  (add-hook 'go-mode-hook 'lsp-deferred)
  (add-hook 'go-mode-hook 'subword-mode)
  (add-hook 'go-mode-hook 'pd/configure-go-mode)

  (-if-let (goimports (executable-find "goimports"))
      (setq gofmt-command goimports))

  (bind-keys :map go-mode-map
             ("C-c h"   . lsp-ui-doc-show)
             ("C-c t ." . go-test-current-test)
             ("C-c t t" . go-test-current-file)
             ("C-c t f" . go-test-current-file)
             ("C-c t p" . go-test-current-project)))

(after 'go-playground
  (bind-key "C-c C-c" 'go-playground-save-and-run go-playground-mode-map)
  (bind-key "C-c C-k" 'go-playground-remove-current-snippet go-playground-mode-map))

(provide 'pd/go)
