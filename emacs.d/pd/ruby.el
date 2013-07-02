(after 'ruby-mode
  (add-hook 'ruby-mode-hook 'ruby-tools-mode)
  (add-hook 'ruby-mode-hook 'subword-mode)
  (add-hook 'ruby-mode-hook 'yard-mode)
  (add-hook 'ruby-mode-hook 'eldoc-mode)
  (add-hook 'ruby-mode-hook 'robe-mode)
  (add-hook 'ruby-mode-hook 'repl-toggle-mode)
  (add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)

  (global-rinari-mode +1)
  (defun rinari-script-path ()
    "Override to check RAILS_ROOT/bin instead of RAILS_ROOT/script"
    (concat (file-name-as-directory (expand-file-name "bin" (rinari-root))))))

(after 'inf-ruby
  (add-hook 'inf-ruby-mode-hook 'pd/comint-disable-echo)
  (bind-key "C-c e b" 'ruby-send-file inf-ruby-minor-mode-map)
  (bind-key "C-c e s" 'ruby-send-last-sexp inf-ruby-minor-mode-map)
  (bind-key "C-c e d" 'ruby-send-definition inf-ruby-minor-mode-map)
  (bind-key "C-c e R" 'ruby-send-region-and-go inf-ruby-minor-mode-map)
  (bind-key "C-c e D" 'ruby-send-definition-and-go inf-ruby-minor-mode-map))

(after 'robe
  (push 'ac-source-robe ac-sources))

; I never really got this thing working well enough to publish it.
; Maybe some day.
(autoload 'find-gem "~/sauce/find-gem.el/find-gem.el"
  "Open a gem from your current rbenv / rvm / ruby gemset"
  'interactive)
(bind-key "C-c j g" 'find-gem)

(provide 'pd/ruby)
