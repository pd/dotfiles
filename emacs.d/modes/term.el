(require 'multi-term)
(setq multi-term-program "/bin/zsh")
(add-hook 'term-mode-hook '(lambda ()
                             (setq show-trailing-whitespace nil)))
