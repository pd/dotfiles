(after 'comint
  (pd/load-ext 'comint)
  (add-hook 'comint-mode-hook 'ansi-color-for-comint-mode-on)
  (bind-key "C-c M-o" 'pd/comint-truncate-buffer comint-mode-map))

(after 'pcomplete
  (require 'pd/pcmpl-powify)
  (defalias 'pcomplete/g 'pcomplete/git))

(after 'shell
  (pd/load-ext 'shell)
  (setq shell-prompt-pattern "^[^\n]*[#$%>»] *")
  (add-hook 'shell-mode-hook 'pcomplete-shell-setup)
  (add-hook 'shell-mode-hook 'pd/comint-disable-echo)
  (add-hook 'shell-mode-hook 'pd/shell-set-up-dirtrack)
  (bind-key "C-c d" 'dirs shell-mode-map))

(after 'shell-switcher
  (shell-switcher-mode)
  (setq shell-switcher-new-shell-function 'shell-switcher-make-shell)
  (add-hook 'shell-mode-hook 'shell-switcher-manually-register-shell))

;; Actually editing .sh files.
(after 'sh-script
  (setq-default sh-basic-offset 2
                sh-indentation 2))

(provide 'pd/shell)
