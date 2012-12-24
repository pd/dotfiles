(require 'eldoc)
(require 'paredit)

(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'enable-paredit-mode)
