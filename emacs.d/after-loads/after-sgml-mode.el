(require 'zencoding-mode)
(require 'tagedit)

(tagedit-add-paredit-like-keybindings)
(keydef (tagedit "s-k") windmove-up)
(keydef (tagedit "s-K") tagedit-kill-attribute)

(setq zencoding-insert-flash-time 0.01
      zencoding-indentation 2)

(add-hook 'sgml-mode-hook 'zencoding-mode)
(add-hook 'html-mode-hook 'tagedit-mode)

(keydef (sgml "C-c e") zencoding-expand-line)
