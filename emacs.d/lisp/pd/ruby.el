(after 'enh-ruby-mode
  (require 'chruby)
  (require 'bundler)

  (setq enh-ruby-deep-arglist nil
        enh-ruby-deep-indent-paren nil
        enh-ruby-deep-indent-paren-style nil
        enh-ruby-add-encoding-comment-on-save nil)

  (defun pd/flycheck-disable-rubocop ()
    "I really don't want generic rubocop warnings at all times."
    (add-to-list 'flycheck-disabled-checkers 'ruby-rubocop))

  (add-hook 'enh-ruby-mode-hook 'ruby-tools-mode)
  (add-hook 'enh-ruby-mode-hook 'subword-mode)
  (add-hook 'enh-ruby-mode-hook 'yard-mode)
  (add-hook 'enh-ruby-mode-hook 'eldoc-mode)
  (add-hook 'enh-ruby-mode-hook 'repl-toggle-mode)
  (add-hook 'enh-ruby-mode-hook 'inf-ruby-minor-mode)
  (add-hook 'enh-ruby-mode-hook 'pd/flycheck-disable-rubocop)

  ; reclaim some bindings enh-ruby-mode clobbers
  (pd/enable-newline-and-indent enh-ruby-mode-map)
  (unbind-key "C-c /" enh-ruby-mode-map)

  (setq seeing-is-believing-prefix "C-c \\")
  (require 'seeing-is-believing)

  (require 'f)
  (when (f-exists-p (f-expand "~/.ruby-version"))
    (let ((default-directory (f-expand "~")))
      (chruby-use-corresponding)
      (setq enh-ruby-program (executable-find "ruby")))))

(after 'inf-ruby
  (add-hook 'inf-ruby-mode-hook 'pd/comint-disable-echo)
  (bind-keys :map inf-ruby-minor-mode-map
             ("C-c e s" . ruby-send-last-sexp)
             ("C-c e d" . ruby-send-definition)
             ("C-c e R" . ruby-send-region-and-go)
             ("C-c e D" . ruby-send-definition-and-go)))

(after 'rspec-mode
  (setq rspec-use-rake-when-possible nil))

(after 'seeing-is-believing
  (add-hook 'enh-ruby-mode-hook 'seeing-is-believing))

; I never really got this thing working well enough to publish it.
; Maybe some day.
(autoload 'find-gem "~/sauce/el/find-gem.el/find-gem.el"
  "Open a gem from your current rbenv / rvm / ruby gemset"
  'interactive)
(bind-key "C-c j g" 'find-gem)

(provide 'pd/ruby)
