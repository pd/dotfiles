(when (string-equal "gnu/linux" system-type)
  (when (file-executable-p "/usr/bin/zsh")
    (setenv "SHELL" "/usr/bin/zsh")
    (setq shell-file-name "/usr/bin/zsh"))

  (require 'exec-path-from-shell)
  (add-to-list 'exec-path-from-shell-variables "GOPATH")
  (exec-path-from-shell-initialize)

  ;; pacaur -S ttf-roboto-mono
  (set-face-attribute 'default nil :height 80 :family "Roboto Mono"))

(provide 'pd/linux)
