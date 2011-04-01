(if (load "haskell-site-file" 'noerror 'nomessage)
    (progn
      (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
      (add-hook 'haskell-mode-hook 'turn-on-font-lock)
      (add-hook 'haskell-mode-hook 'pd/run-coding-hook))
  (message "Haskell mode unavailable"))

(provide 'pd/haskell)
