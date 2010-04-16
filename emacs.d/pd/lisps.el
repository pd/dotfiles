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

; dunno where else to put this atm
(defun pd/lein-swank ()
  (interactive)
  (let ((default-directory (locate-dominating-file (buffer-file-name) "project.clj"))
        (explicit-shell-file-name "lein")
        (explicit-lein-args '("swank")))
    (when default-directory
      (shell "*lein-swank*"))))


(provide 'pd/lisps)