(defun pd/js2-mode-name ()
  (setq mode-name "js2"))

(after 'abbrev
  (diminish 'abbrev-mode))

(after 'company
  (diminish 'company-mode))

(after 'eldoc
  (diminish 'eldoc-mode))

(after 'elisp-slime-nav
  (diminish 'elisp-slime-nav-mode))

(after 'fic-ext-mode
  (diminish 'fic-ext-mode))

(after 'handlebars-sgml-mode
  (handlebars-use-mode 'minor)
  (diminish 'handlebars-sgml-minor-mode " hbs"))

(after 'js2-mode
  (add-hook 'js2-mode-hook 'pd/js2-mode-name))

(after 'quickref
  (diminish 'quickref-mode))

(after 'ruby-tools
  (diminish 'ruby-tools-mode))

(after 'repl-toggle
  (diminish 'repl-toggle-mode))

(after 'paredit
  (diminish 'paredit-mode))

(after 'projectile
  (setq projectile-mode-line '(:eval (format " prj[%s]" (projectile-project-name)))))

(after 'projectile-rails
  (diminish 'projectile-rails-mode))

(after 'subword
  (diminish 'subword-mode))

(after 'which-key
  (diminish 'which-key-mode))

(after 'wrap-region
  (diminish 'wrap-region-mode))

(after 'yard-mode
  (diminish 'yard-mode))

(provide 'pd/diminish)
