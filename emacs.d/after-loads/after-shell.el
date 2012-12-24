(require 'ansi-color)
(require 'dirtrack)
(require 'pcmpl-git)
(require 'pd/shell)
(require 'pd/pcmpl-powify)

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'shell-mode-hook 'pd/turn-off-show-trailing-whitespace)
(add-hook 'shell-mode-hook 'pd/enable-shell-mode-bindings)
(add-hook 'shell-mode-hook 'pd/enable-dirtrack)
(add-hook 'shell-mode-hook 'pcomplete-shell-setup)
(add-hook 'shell-mode-hook 'pd/turn-off-comint-echo)

; to test why the fuck your prompt doesn't match:
; dirtrack-debug-mode
; defadvice on dirtrack to set a tmp var to the given input (or edit dirtrack.el ...)
; and now easily iterate on the regexp:
; (s-match "\\`%?[\r\n ]*\\([^\n ]+\\).* » \\'" pd-dirtrack-input-test)
(setq shell-prompt-pattern "^[^\n]*[#$%>»] *")
(add-hook 'dirtrack-directory-change-hook 'pd/dirtrack-directory-change-hook)
