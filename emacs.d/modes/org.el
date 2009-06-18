(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(global-set-key (kbd "C-c a") 'org-agenda)

(setq org-directory "~/org/"
      org-agenda-files (directory-files org-directory 'full "\\.org$"))

(setq org-log-done 'time
      org-replace-disputed-keys t
      org-completion-use-ido t
      org-hide-leading-stars t
      org-odd-levels-only t
      org-startup-folded nil
      org-special-ctrl-a/e t
      org-special-ctrl-k t
      org-agenda-restore-windows-after-quit t)

(setq org-todo-keywords
      '((sequence "TODO(t)" "WISH(w)" "|"
                  "DONE(d)" "CANCELLED(c)")))

(setq org-todo-keyword-faces
      '(("TODO" :foreground "green" :weight bold)
        ("WISH" :foreground "orange" :weight bold)
        ("DONE" :foreground "blue" :weight bold)
        ("CANCELLED" :foreground "light grey" :weight bold)))
