; ack should search all files, and ignore case. i am lazy.
(eval-after-load 'full-ack
  '(progn
     (setq ack-ignore-case t
           ack-arguments (list "-a"))))

(provide 'pd/ack)
