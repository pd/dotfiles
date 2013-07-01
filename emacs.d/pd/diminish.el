(defun pd/js2-mode-name ()
  (setq mode-name "js2"))

(after 'eldoc
  (diminish 'eldoc-mode))

(after 'elisp-slime-nav
  (diminish 'elisp-slime-nav-mode))

(after 'js2-mode
  (add-hook 'js2-mode-hook 'pd/js2-mode-name))

(after 'quickref
  (diminish 'quickref-mode))

(after 'paredit
  (diminish 'paredit-mode))

(after 'subword
  (diminish 'subword-mode))

(after 'wrap-region
  (diminish 'wrap-region-mode))

(after 'auto-complete
  (diminish 'auto-complete-mode))

(after 'fic-ext-mode
  (diminish 'fic-ext-mode))

(after 'handlebars-sgml-mode
  (handlebars-use-mode 'minor)
  (diminish 'handlebars-sgml-minor-mode " hbs"))

(provide 'pd/diminish)
