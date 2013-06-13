(after 'comint
  (pd/load-ext 'comint)
  (add-hook 'comint-mode-hook 'ansi-color-for-comint-mode-on)
  (bind-key "C-c M-o" 'pd/comint-truncate-buffer comint-mode-map))

(after 'shell-switcher
  (shell-switcher-mode)
  (setq shell-switcher-new-shell-function 'shell-switcher-make-shell)
  (add-hook 'shell-mode-hook 'shell-switcher-manually-register-shell))

(defun pd/shell-directory-tracking ()
  "dirtrack > shell-dirtrack for serious"
  (shell-dirtrack-mode -1)
  (dirtrack-mode +1)
  (setq dirtrack-list '("\\`%?[\r\n ]*\\([^\n ]+\\) .*» \\'" 1)))

(after 'shell
  (setq shell-prompt-pattern "^[^\n]*[#$%>»] *")
  (bind-key "C-c d" 'dirs shell-mode-map)

  (add-hook 'shell-mode-hook 'pcomplete-shell-setup)
  (add-hook 'shell-mode-hook 'pd/comint-disable-echo)
  (add-hook 'shell-mode-hook 'pd/shell-directory-tracking)

  ; After the shell is running, pretend the directory changed for the first time.
  (add-hook 'shell-mode-hook 'pd/dirtrack-directory-changed))

(after 'pcomplete
  (require 'pd/pcmpl-powify)
  (defalias 'pcomplete/g 'pcomplete/git))

(after 'dirtrack
  (add-hook 'dirtrack-directory-change-hook 'pd/dirtrack-directory-changed))

;; Actually editing .sh files.
(after 'sh-script
  (setq-default sh-basic-offset 2
                sh-indentation 2))

(provide 'pd/shell)
