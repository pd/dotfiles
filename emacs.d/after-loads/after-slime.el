(setq slime-protocol-version 'ignore
      slime-net-coding-system 'utf-8-unix)

(setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)

(require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))
