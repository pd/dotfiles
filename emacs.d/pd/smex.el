; smex should always be loaded last, as it catalogs all
; the available commands at the time of its loading.
; use smex-update to refresh the list.
(require 'smex)
(smex-initialize)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c M-x") 'smex-update-and-run)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(setq smex-save-file "~/.emacs.d/.crap/smex.save")
(smex-auto-update 120) ; auto update after 2 minutes idle

(provide 'pd/smex)
