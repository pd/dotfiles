(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(add-hook 'org-mode-hook
          (lambda ()
            (setq org-log-done 'time)))
