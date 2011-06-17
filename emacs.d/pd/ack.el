; ack should search all files, and ignore case. i am lazy.
(require 'full-ack)
(setq ack-ignore-case t
      ack-arguments '("-a")
      ack-executable (or (executable-find "ack")
                         (executable-find "ack-grep")))

(provide 'pd/ack)
