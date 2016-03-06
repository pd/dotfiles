(when (string-equal "gnu/linux" system-type)
  (when (file-executable-p "/usr/bin/zsh")
    (setenv "SHELL" "/usr/bin/zsh")
    (setq shell-file-name "/usr/bin/zsh"))

  (require 'exec-path-from-shell)
  (add-to-list 'exec-path-from-shell-variables "GOPATH")
  (exec-path-from-shell-initialize)

  (set-face-attribute 'default nil :height 80 :family "Source Code Pro Medium"))

(provide 'pd/linux)
