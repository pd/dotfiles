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
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'clojure-mode-hook 'pd/run-lisp-hook)
(add-hook 'slime-repl-mode-hook 'pd/run-lisp-hook)

; I don't need TAGS when I'm looking for an elisp function.
(define-key emacs-lisp-mode-map (kbd "M-.") 'find-function-at-point)

; slime-mode steals M-p and M-n to navigate compilation error
; notes. i don't care about those. please give me paragraph
; movement back.
(defun pd/slime-mode-paragraph-movement ()
  (pd/restore-paragraph-movement slime-mode-map))

(add-hook 'slime-mode-hook 'pd/restore-paragraph-movement)
(add-hook 'slime-repl-mode-hook 'clojure-mode-font-lock-setup)

(provide 'pd/lisps)
