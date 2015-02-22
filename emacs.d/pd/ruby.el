(after 'enh-ruby-mode
  (require 'chruby)
  (require 'bundler)

  (setq enh-ruby-deep-arglist nil
        enh-ruby-deep-indent-paren nil
        enh-ruby-deep-indent-paren-style nil)

  (add-hook 'enh-ruby-mode-hook 'ruby-tools-mode)
  (add-hook 'enh-ruby-mode-hook 'subword-mode)
  (add-hook 'enh-ruby-mode-hook 'yard-mode)
  (add-hook 'enh-ruby-mode-hook 'eldoc-mode)
  (add-hook 'enh-ruby-mode-hook 'repl-toggle-mode)
  (add-hook 'enh-ruby-mode-hook 'inf-ruby-minor-mode)
  (add-hook 'enh-ruby-mode-hook 'projectile-mode)

  ; reclaim some bindings enh-ruby-mode clobbers
  (bind-key "RET" 'newline-and-indent enh-ruby-mode-map)
  (unbind-key "C-c /" enh-ruby-mode-map))

(after 'inf-ruby
  (add-hook 'inf-ruby-mode-hook 'pd/comint-disable-echo)

  ;; (after 'auto-complete
  ;;   (add-to-list 'ac-modes 'inf-ruby-mode))

  (bind-key "C-c e s" 'ruby-send-last-sexp inf-ruby-minor-mode-map)
  (bind-key "C-c e d" 'ruby-send-definition inf-ruby-minor-mode-map)
  (bind-key "C-c e R" 'ruby-send-region-and-go inf-ruby-minor-mode-map)
  (bind-key "C-c e D" 'ruby-send-definition-and-go inf-ruby-minor-mode-map))

(after 'projectile
  (add-hook 'projectile-mode-hook 'projectile-rails-on))

(after 'projectile-rails
  (bind-key "s-m" 'projectile-rails-find-model projectile-rails-mode-map)
  (bind-key "s-c" 'projectile-rails-find-controller projectile-rails-mode-map)
  (bind-key "s-s" 'projectile-rails-find-spec projectile-rails-mode-map)
  (bind-key "s-r" 'projectile-rails-goto-routes projectile-rails-mode-map)
  (bind-key "s-t" 'projectile-rails-goto-spec-helper projectile-rails-mode-map))

; I never really got this thing working well enough to publish it.
; Maybe some day.
(autoload 'find-gem "~/sauce/el/find-gem.el/find-gem.el"
  "Open a gem from your current rbenv / rvm / ruby gemset"
  'interactive)
(bind-key "C-c j g" 'find-gem)

(provide 'pd/ruby)
