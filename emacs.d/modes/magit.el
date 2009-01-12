(autoload 'magit-status "magit" "Major mode for git interaction" t)

(defun magit-insert-submodule-summary ()
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (insert "\n" (magit-shell "git submodule summary"))))

(add-hook 'magit-log-edit-mode-hook
          (lambda ()
            (define-key magit-log-edit-map (kbd "C-M-s") 'magit-insert-submodule-summary)))
