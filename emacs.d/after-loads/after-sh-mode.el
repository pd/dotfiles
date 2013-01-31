(defun pd/setup-sh-mode ()
  (setq sh-basic-offset 2
        sh-indentation  2))

(add-hook 'sh-mode-hook 'pd/setup-sh-mode)
