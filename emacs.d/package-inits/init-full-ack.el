(setq ack-ignore-case t
      ack-arguments '("-a")
      ack-executable (or (executable-find "ack")
                         (executable-find "ack-grep")))
