(require 'ansi-color)
(require 'dirtrack)

(setq shell-prompt-pattern "^[^\n]*[#$%>»] *")

(defun pd/enable-shell-mode-bindings ()
  (define-key shell-mode-map (kbd "C-c d")   'dirs)
  (define-key shell-mode-map (kbd "C-c s")   'pd/smart-shell)
  (define-key shell-mode-map (kbd "C-c M-s") 'pd/ido-switch-shell)
  (define-key shell-mode-map (kbd "C-c DEL") 'pd/clear-shell))

(defun pd/enable-dirtrack ()
  (dirtrack-mode 1)
  (setq dirtrack-list '("^\\([^\n ]+\\)\\( @ .+\\) »" 1)))

(defadvice shell-dirstack-message (after pd/shell-mode-buffer-name first activate)
  (let* ((cwd (directory-file-name (abbreviate-file-name default-directory)))
         (new-name (concat "*shell: " cwd "*")))
    (rename-buffer new-name t)))

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'shell-mode-hook 'pd/turn-off-show-trailing-whitespace)
(add-hook 'shell-mode-hook 'pd/enable-shell-mode-bindings)
(add-hook 'shell-mode-hook 'pd/enable-dirtrack)

(setq term-prompt-regexp "^[^\n]*[#$%>»] *"
      term-ansi-buffer-base-name t)
(add-hook 'term-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'term-mode-hook 'pd/turn-off-show-trailing-whitespace)

; dunno where else to put this really.
(add-hook 'comint-mode-hook 'pd/turn-off-show-trailing-whitespace)

(provide 'pd/shell)
