(defvar pd/lisp-hook nil)

(defun pd/add-lisp-shared-keybindings ()
  (pd/enable-newline-and-indent lisp-mode-shared-map))

(add-hook 'pd/lisp-hook 'pd/run-coding-hook)
(add-hook 'pd/lisp-hook 'pd/turn-on-paredit-mode)
(add-hook 'pd/lisp-hook 'pd/turn-on-show-paren-mode)
(add-hook 'pd/lisp-hook 'pd/add-lisp-shared-keybindings)

(defun pd/run-lisp-hook ()
  (run-hooks 'pd/lisp-hook))

(add-hook 'emacs-lisp-mode-hook 'pd/run-lisp-hook)
(add-hook 'clojure-mode-hook 'pd/run-lisp-hook)
(add-hook 'slime-repl-mode-hook 'pd/run-lisp-hook)

; slime-mode overrides these for slime-next-note, slime-previous-note
; i don't know what that is so please give me my pararaph nav back
(defun pd/restore-paragraph-movement ()
  (define-key slime-mode-map (kbd "M-p") 'backward-paragraph)
  (define-key slime-mode-map (kbd "M-n") 'forward-paragraph))

(add-hook 'slime-mode-hook 'pd/restore-paragraph-movement)

(provide 'pd/lisps)
