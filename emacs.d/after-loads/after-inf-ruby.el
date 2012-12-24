(setq inf-ruby-first-prompt-pattern "^.*[0-9:]+ *[>*\"'] *"
      inf-ruby-prompt-pattern "\\(^\\(irb(.*)[0-9:]+[>*\"'] *\\)+\\)\\|\\(^.*:[0-9]+.*> *\\)")

(add-hook 'inf-ruby-mode-hook 'pd/turn-off-comint-echo)
