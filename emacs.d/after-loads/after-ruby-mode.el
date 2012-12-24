(defun pd/add-ruby-keybindings ()
  (pd/enable-newline-and-indent ruby-mode-map))

(add-hook 'ruby-mode-hook 'pd/run-coding-hook)
(add-hook 'ruby-mode-hook 'pd/add-ruby-keybindings)
(add-hook 'ruby-mode-hook 'ruby-tools-mode)
(add-hook 'ruby-mode-hook 'subword-mode)
(add-hook 'ruby-mode-hook 'yard-mode)
(add-hook 'ruby-mode-hook 'eldoc-mode)
