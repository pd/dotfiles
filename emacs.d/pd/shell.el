(require 'ansi-color)
(require 'dirtrack)

(setq shell-prompt-pattern "^[^\n]*[#$%>»] *")

(defun pd/enable-shell-mode-bindings ()
  (define-key shell-mode-map (kbd "C-c d")   'dirs)
  (define-key shell-mode-map (kbd "C-c s")   'pd/smart-shell)
  (define-key shell-mode-map (kbd "C-c M-s") 'pd/ido-switch-shell)
  (define-key shell-mode-map (kbd "C-c DEL") 'pd/clear-shell))

(setq directory-abbrev-alist
      (mapcar (lambda (alias) (cons (cdr alias) (car alias)))
              (pd/zsh-dir-aliases)))

(setq pd/zsh-dir-aliases-cache (pd/zsh-dir-aliases))

(defun pd/dirtrack-directory-function (path)
  (reduce (lambda (str alias)
            (s-replace (car alias) (cdr alias) str))
          pd/zsh-dir-aliases-cache
          :initial-value path))

(defun pd/dirtrack-directory-change-hook ()
  (let* ((cwd (directory-file-name (abbreviate-file-name default-directory)))
         (new-name (concat "*shell: " cwd "*")))
    (rename-buffer new-name t)))

(defun pd/enable-dirtrack ()
  (dirtrack-mode 1)
  (shell-dirtrack-mode -1)
  (setq dirtrack-list '("\\`%?[\r\n ]*\\([^\n ]+\\) .*» \\'" 1))
  (setq dirtrack-directory-function 'pd/dirtrack-directory-function))

; to test why the fuck your prompt doesn't match:
; dirtrack-debug-mode
; defadvice on dirtrack to set a tmp var to the given input (or edit dirtrack.el ...)
; and now easily iterate on the regexp:
; (s-match "\\`%?[\r\n ]*\\([^\n ]+\\).* » \\'" pd-dirtrack-input-test)

(add-hook 'dirtrack-directory-change-hook 'pd/dirtrack-directory-change-hook)

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'shell-mode-hook 'pd/turn-off-show-trailing-whitespace)
(add-hook 'shell-mode-hook 'pd/enable-shell-mode-bindings)
(add-hook 'shell-mode-hook 'pd/enable-dirtrack)

; dunno where else to put this really.
(add-hook 'comint-mode-hook 'pd/turn-off-show-trailing-whitespace)

(provide 'pd/shell)
