(defun pd/add-ruby-keybindings ()
  (pd/enable-newline-and-indent ruby-mode-map))

(add-hook 'ruby-mode-hook 'pd/run-coding-hook)
(add-hook 'ruby-mode-hook 'pd/add-ruby-keybindings)

(require 'inf-ruby-bond)

(provide 'pd/ruby)
