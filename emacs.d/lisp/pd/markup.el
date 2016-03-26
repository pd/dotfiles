(after 'css-mode
  (setq css-indent-offset 2))

(after 'markdown-mode
  (require 'markdown-toc nil 'noerror))

(after 'web-mode
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2)
  (bind-key "C-c e" 'emmet-expand-line web-mode-map))

(provide 'pd/markup)
