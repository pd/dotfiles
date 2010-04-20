(require 'ansi-color)

(setq shell-prompt-pattern "^[^\n]*[#$%>»] *")

(defun pd/enable-ansi-color-for-comint ()
  (ansi-color-for-comint-mode-on))

(add-hook 'shell-mode-hook 'pd/enable-ansi-color-for-comint)
(add-hook 'shell-mode-hook 'pd/turn-off-show-trailing-whitespace)

(provide 'pd/shell)
