(setq inf-ruby-first-prompt-pattern "^[^ ]+> *"
      inf-ruby-prompt-pattern "^[^ ]+> *")

(add-hook 'inf-ruby-mode-hook 'pd/turn-off-comint-echo)
