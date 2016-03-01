(after 'css-mode
  (setq css-indent-offset 2))

(after 'markdown-mode
  (require 'markdown-toc nil 'noerror))

(after 'web-mode
  (bind-key "C-c e" 'emmet-expand-line web-mode-map))

(provide 'pd/markup)
