(when (package-installed-p 'yasnippet-bundle)
  (setq yas/root-directory "~/.emacs.d/snippets")
  (yas/load-directory yas/root-directory))

(defun 'pd/turn-on-yasnippet ()
  (yas/minor-mode t))

(provide 'pd/yasnippet)
