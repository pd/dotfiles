(require 'f)
(require 'dash)

(defun pd/configure-go-mode ()
  (setq tab-width 4)
  (set (make-local-variable 'company-backends) '(company-go)))

(after 'go-mode
  (require 'company-go)
  (require 'go-direx)

  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook 'go-eldoc-setup)
  (add-hook 'go-mode-hook 'subword-mode)
  (add-hook 'go-mode-hook 'pd/configure-go-mode)

  (-if-let (goimports (executable-find "goimports"))
      (setq gofmt-command goimports))

  (bind-keys :map go-mode-map
             ("M-."     . godef-jump)
             ("C-c d"   . go-direx-pop-to-buffer)
             ("C-c h"   . godoc-at-point)
             ("C-c t ." . go-test-current-test)
             ("C-c t t" . go-test-current-file)
             ("C-c t f" . go-test-current-file)
             ("C-c t p" . go-test-current-project)))

(after 'go-playground
  (bind-key "C-c C-c" 'go-playground-save-and-run go-playground-mode-map)
  (bind-key "C-c C-k" 'go-playground-remove-current-snippet go-playground-mode-map))

(provide 'pd/go)
