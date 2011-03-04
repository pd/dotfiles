(when (package-installed-p 'yasnippet-bundle)
  (setq yas/root-directory "~/.emacs.d/snippets"
        yas/prompt-functions '(yas/ido-prompt
                               yas/x-prompt
                               yas/completing-prompt))
  (yas/load-directory yas/root-directory))

(add-to-list 'auto-mode-alist '("\\.yasnippet\\'" . snippet-mode))

(provide 'pd/yasnippet)
