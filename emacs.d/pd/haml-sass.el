(autoload 'haml-mode "haml-mode" "Major mode for haml" t)
(autoload 'sass-mode "sass-mode" "Major mode for sass" t)
(setq auto-mode-alist
      (append auto-mode-alist
              '(("\\.haml\\'" . haml-mode)
                ("\\.sass\\'" . sass-mode))))

(provide 'pd/haml-sass)
