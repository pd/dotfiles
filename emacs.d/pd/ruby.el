(defun pd/add-ruby-keybindings ()
  (pd/enable-newline-and-indent ruby-mode-map))

(add-hook 'ruby-mode-hook 'pd/run-coding-hook)
(add-hook 'ruby-mode-hook 'pd/add-ruby-keybindings)

(setq inf-ruby-first-prompt-pattern "^.*[0-9:]+ *[>*\"'] *"
      inf-ruby-prompt-pattern "\\(^\\(irb(.*)[0-9:]+[>*\"'] *\\)+\\)\\|\\(^.*:[0-9]+.*> *\\)")

(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile\\'" . ruby-mode))

(require 'inf-ruby-bond)

(provide 'pd/ruby)
