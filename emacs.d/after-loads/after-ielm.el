(require 'eldoc)
(require 'paredit)

(add-hook 'ielm-mode-hook 'pd/lisp-mode)
(add-hook 'ielm-mode-hook 'elisp-slime-nav-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
