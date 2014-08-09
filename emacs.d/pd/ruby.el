(after 'enh-ruby-mode
  (add-hook 'enh-ruby-mode-hook 'ruby-tools-mode)
  (add-hook 'enh-ruby-mode-hook 'subword-mode)
  (add-hook 'enh-ruby-mode-hook 'yard-mode)
  (add-hook 'enh-ruby-mode-hook 'eldoc-mode)
  (add-hook 'enh-ruby-mode-hook 'repl-toggle-mode)
  (add-hook 'enh-ruby-mode-hook 'inf-ruby-minor-mode))

(after 'inf-ruby
  (add-hook 'inf-ruby-mode-hook 'pd/comint-disable-echo)

  (after 'auto-complete
    (add-to-list 'ac-modes 'inf-ruby-mode))

  (bind-key "C-c e s" 'ruby-send-last-sexp inf-ruby-minor-mode-map)
  (bind-key "C-c e d" 'ruby-send-definition inf-ruby-minor-mode-map)
  (bind-key "C-c e R" 'ruby-send-region-and-go inf-ruby-minor-mode-map)
  (bind-key "C-c e D" 'ruby-send-definition-and-go inf-ruby-minor-mode-map))

; I never really got this thing working well enough to publish it.
; Maybe some day.
(autoload 'find-gem "~/sauce/el/find-gem.el/find-gem.el"
  "Open a gem from your current rbenv / rvm / ruby gemset"
  'interactive)
(bind-key "C-c j g" 'find-gem)

(provide 'pd/ruby)
