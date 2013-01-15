(setq inf-ruby-first-prompt-pattern "^.*pry\\(([^)]+)\\)?[>*\"'] *"
      inf-ruby-prompt-pattern "^.*pry\\(([^)]+)\\)?[>*\"'] *")

(add-hook 'inf-ruby-mode-hook 'pd/turn-off-comint-echo)
