(require 'ansi-color)
(require 'dirtrack)
(require 'pcmpl-git)
(require 'pd/pcmpl-powify)

(defun pd/truncate-shell-buffer ()
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

(keydef (shell "C-c d")   dirs)
(keydef (shell "C-c M-o") pd/truncate-shell-buffer)

(defun pd/dirtrack-directory-function (path)
  "See `dirtrack-directory-function'."
  (pd/expand-file-name path))

(defun pd/dirtrack-directory-change-hook ()
  "Updates smart shell buffer names to reflect their current working directory."
  (when (and (eq 'shell-mode (current-buffer))
             (string-match-p "^\*shell" (buffer-name (current-buffer))))
    (let* ((cwd (directory-file-name (pd/abbreviate-file-name default-directory))))
      (rename-buffer (concat "*shell: " cwd "*") t))))

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
