(autoload 'magit-status "magit" "Major mode for git interaction" t)

(defun magit-insert-submodule-summary ()
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (insert "\n" (magit-shell "git submodule summary"))))

(add-hook 'magit-log-edit-mode-hook
          (lambda ()
            (define-key magit-log-edit-map (kbd "C-M-s") 'magit-insert-submodule-summary)))

(eval-after-load 'magit
  '(progn
     (set-face-background 'magit-item-highlight "gray12")
     (set-face-foreground 'magit-diff-add "green3")
     (set-face-foreground 'magit-diff-del "red3")))
