(require 'ansi-color)

(setq shell-prompt-pattern "^[^\n]*[#$%>»] *")

(defun pd/enable-shell-mode-bindings ()
  (define-key shell-mode-map (kbd "C-c d") 'dirs))

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'shell-mode-hook 'pd/turn-off-show-trailing-whitespace)
(add-hook 'shell-mode-hook 'pd/enable-shell-mode-bindings)

(setq term-prompt-regexp "^[^\n]*[#$%>»] *"
      term-ansi-buffer-base-name t)
(add-hook 'term-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'term-mode-hook 'pd/turn-off-show-trailing-whitespace)

(defun pd/term-buffer-name ()
  (find-if (lambda (b)
             (save-excursion
               (set-buffer b)
               (string= "term-mode" (symbol-name major-mode))))
           (buffer-list)))

(defun pd/term-buffer-exists ()
  (not (eq ()
           (pd/term-buffer-name))))

(defun pd/term (create-new)
  (interactive "P")
  (let ((current-term (pd/term-buffer-name)))
    (if (and current-term (not create-new))
        (switch-to-buffer current-term)
      (ansi-term (or (getenv "SHELL") "/bin/zsh")))))

(provide 'pd/shell)
