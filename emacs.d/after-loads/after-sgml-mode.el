(require 'zencoding)
(setq zencoding-insert-flash-time 0.01
      zencoding-indentation 2)
(add-hook 'sgml-mode-hook 'zencoding-mode)
(keydef (sgml "C-c e") zencoding-expand-line)
