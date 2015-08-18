(require 'f)
(require 'dash)

(defun pd/configure-go-mode ()
  (setq tab-width 4)
  (set (make-local-variable 'company-backends) '(company-go)))

(defun pd/enable-go-oracle ()
  "If $GOPATH is set, and oracle.el is available in it, add `go-oracle-mode'
to the `go-mode-hook'."
  (-if-let* ((gopath (getenv "GOPATH"))
             (oracle-el (f-join gopath "src/golang.org/x/tools/cmd/oracle/oracle.el"))
             (oracle-exec (executable-find "oracle")))
      (when (f-exists? oracle-el)
        (add-to-list 'load-path (f-parent oracle-el))
        (require 'go-oracle oracle-el)
        (setq go-oracle-command oracle-exec))))

(after 'go-mode
  (require 'company-go)
  (require 'go-direx)
  (pd/enable-go-oracle)

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
             ("C-c t p" . go-test-current-project))

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
      nil)))

(provide 'pd/go)
