(require 'ansi-color)

(setq shell-prompt-pattern "^[^\n]*[#$%>»] *")

(defun pd/enable-shell-mode-bindings ()
  (define-key shell-mode-map (kbd "C-c d") 'dirs))

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'shell-mode-hook 'pd/turn-off-show-trailing-whitespace)
(add-hook 'shell-mode-hook 'pd/enable-shell-mode-bindings)

(provide 'pd/shell)
