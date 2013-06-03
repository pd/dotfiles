(defun pd/shell-set-up-dirtrack ()
  (dirtrack-mode +1)
  (shell-dirtrack-mode -1)
  (setq dirtrack-list '("\\`%?[\r\n ]*\\([^\n ]+\\) .*» \\'" 1)))
