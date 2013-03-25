(require 'ansi-color)
(require 'dirtrack)
(require 'pcmpl-git)
(require 'pd/pcmpl-powify)
(require 'pd/smart-shell)

(defun pd/truncate-shell-buffer ()
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

(keydef (shell "C-c d")   dirs)
(keydef (shell "C-c s")   pd/smart-shell)
(keydef (shell "C-c M-s") pd/ido-switch-shell)
(keydef (shell "C-c DEL") pd/truncate-shell-buffer)

(defun pd/dirtrack-directory-function (path)
  (pd/expand-file-name path))

(defun pd/dirtrack-directory-change-hook ()
  (let* ((cwd (directory-file-name (pd/abbreviate-file-name default-directory)))
         (new-name (concat "*shell: " cwd "*")))
    (rename-buffer new-name t)))

(defun pd/enable-dirtrack ()
  (dirtrack-mode 1)
  (shell-dirtrack-mode -1)
  (setq dirtrack-list '("\\`%?[\r\n ]*\\([^\n ]+\\) .*» \\'" 1))
  (setq dirtrack-directory-function 'pd/dirtrack-directory-function))

(add-hook 'shell-mode-hook 'pd/turn-off-comint-echo)
(add-hook 'shell-mode-hook 'pd/enable-dirtrack)
(add-hook 'shell-mode-hook 'pcomplete-shell-setup)

; to test why the fuck your prompt doesn't match:
; dirtrack-debug-mode
; defadvice on dirtrack to set a tmp var to the given input (or edit dirtrack.el ...)
; and now easily iterate on the regexp:
; (s-match "\\`%?[\r\n ]*\\([^\n ]+\\).* » \\'" pd-dirtrack-input-test)
(setq shell-prompt-pattern "^[^\n]*[#$%>»] *")
(add-hook 'dirtrack-directory-change-hook 'pd/dirtrack-directory-change-hook)
