(after 'enh-ruby-mode
  (require 'chruby)
  (require 'bundler)

  (setq enh-ruby-deep-arglist nil
        enh-ruby-deep-indent-paren nil
        enh-ruby-deep-indent-paren-style nil
        enh-ruby-add-encoding-comment-on-save nil)

  (add-hook 'enh-ruby-mode-hook 'ruby-tools-mode)
  (add-hook 'enh-ruby-mode-hook 'subword-mode)
  (add-hook 'enh-ruby-mode-hook 'yard-mode)
  (add-hook 'enh-ruby-mode-hook 'eldoc-mode)
  (add-hook 'enh-ruby-mode-hook 'repl-toggle-mode)
  (add-hook 'enh-ruby-mode-hook 'inf-ruby-minor-mode)
  (add-hook 'enh-ruby-mode-hook 'projectile-mode)

  ; reclaim some bindings enh-ruby-mode clobbers
  (pd/enable-newline-and-indent enh-ruby-mode-map)
  (unbind-key "C-c /" enh-ruby-mode-map))

(after 'inf-ruby
  (add-hook 'inf-ruby-mode-hook 'pd/comint-disable-echo)
  (bind-keys :map inf-ruby-minor-mode-map
             ("C-c e s" . ruby-send-last-sexp)
             ("C-c e d" . ruby-send-definition)
             ("C-c e R" . ruby-send-region-and-go)
             ("C-c e D" . ruby-send-definition-and-go)))

(after 'projectile
  (add-hook 'projectile-mode-hook 'projectile-rails-on))

(after 'projectile-rails
  (bind-keys :map projectile-rails-mode-map
             ("s-m" . projectile-rails-find-model)
             ("s-c" . projectile-rails-find-controller)
             ("s-s" . projectile-rails-find-spec)
             ("s-r" . projectile-rails-goto-routes)
             ("s-t" . projectile-rails-goto-spec-helper))
  ; I do not want skeleton classes expanded for me.
  (setq projectile-rails-expand-snippet nil))

; I never really got this thing working well enough to publish it.
; Maybe some day.
(autoload 'find-gem "~/sauce/el/find-gem.el/find-gem.el"
  "Open a gem from your current rbenv / rvm / ruby gemset"
  'interactive)
(bind-key "C-c j g" 'find-gem)

(provide 'pd/ruby)
