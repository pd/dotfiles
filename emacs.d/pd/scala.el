(require 'scala-mode-auto)
(require 'ensime)

(defvar pd/scala-hook nil)

(defun pd/add-scala-mode-keybindings ()
  (pd/enable-newline-and-indent scala-mode-map)
  (pd/restore-paragraph-movement ensime-mode-map)
  (define-key ensime-mode-map (kbd "C-c C-p") 'ensime-backward-note)
  (define-key ensime-mode-map (kbd "C-c C-n") 'ensime-forward-note))

(add-hook 'pd/scala-hook 'pd/run-coding-hook)
(add-hook 'pd/scala-hook 'pd/turn-on-show-paren-mode)
(add-hook 'pd/scala-hook 'pd/add-scala-mode-keybindings)

(defun pd/run-scala-hook ()
  (run-hooks 'pd/scala-hook))

(add-hook 'scala-mode-hook 'pd/run-scala-hook)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

(provide 'pd/scala)
