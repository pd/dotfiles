(require 'ansi-color)

(setq shell-prompt-pattern "^[^\n]*[#$%>»] *")

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'shell-mode-hook 'pd/turn-off-show-trailing-whitespace)

(provide 'pd/shell)
